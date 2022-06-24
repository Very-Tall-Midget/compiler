use std::{
    collections::HashMap,
    sync::atomic::{AtomicIsize, AtomicUsize, Ordering},
};

use super::ast::*;

fn get_label() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("_LABEL_{}", COUNTER.fetch_add(1, Ordering::Relaxed))
}

fn get_stack_index() -> &'static AtomicIsize {
    static STACK_INDEX: AtomicIsize = AtomicIsize::new(-8);
    &STACK_INDEX
}

trait ToAssembly {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String>;
}

impl ToAssembly for Program {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = String::from("    .globl main\n");
        res.push_str(self.func.to_assembly(var_map)?.as_str());
        Ok(res)
    }
}

impl ToAssembly for Function {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = format!("{}:\n    push %rbp\n    mov %rsp, %rbp\n", self.name);
        get_stack_index().store(-8, Ordering::SeqCst);
        for expr in &self.body {
            res.push_str(expr.to_assembly(var_map)?.as_str());
        }
        if let Some(Statement::Return(_)) = self.body.last() {
            Ok(res)
        } else {
            res.push_str("    mov $0, %rax\n    mov %rbp, %rsp\n    pop %rbp\n    ret\n");
            Ok(res)
        }
    }
}

impl ToAssembly for Statement {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        match self {
            Statement::Return(expr) => {
                let mut res = expr.to_assembly(var_map)?;
                res.push_str("    mov %rbp, %rsp\n    pop %rbp\n    ret\n");
                Ok(res)
            }
            Statement::Expr(expr) => expr.to_assembly(var_map),
            Statement::Declaration(id, expr_opt) => {
                if var_map.contains_key(id) {
                    Err(format!(
                        "[Code Generation]: Variable '{}' already exists",
                        id
                    ))
                } else {
                    let mut res = String::new();
                    if let Some(expr) = expr_opt {
                        res.push_str(expr.to_assembly(var_map)?.as_str());
                        res.push_str("    push %rax\n");
                    } else {
                        res.push_str("    push $0\n");
                    }
                    let stack_index = get_stack_index().fetch_add(-8, Ordering::SeqCst);
                    var_map.insert(id.clone(), stack_index);
                    Ok(res)
                }
            }
        }
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        match self {
            Expr::Assignment(id, expr) => {
                let mut res = expr.to_assembly(var_map)?;
                if let Some(offset) = var_map.get(id) {
                    res.push_str(format!("    mov %rax, {}(%rbp)\n", offset).as_str());
                    Ok(res)
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Expr::LogOrExpr(loe) => loe.to_assembly(var_map),
        }
    }
}

impl ToAssembly for LogOrExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.log_and_expr.to_assembly(var_map)?;
        for lae in &self.log_and_exprs {
            let clause_label = get_label();
            let end_label = get_label();
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    je {0}\n    mov $1, %rax\n    jmp {1}\n{0}:\n",
                    clause_label, end_label
                )
                .as_str(),
            );
            res.push_str(lae.to_assembly(var_map)?.as_str());
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                    end_label
                )
                .as_str(),
            );
        }
        Ok(res)
    }
}

impl ToAssembly for LogAndExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.bit_or_expr.to_assembly(var_map)?;
        for boe in &self.bit_or_exprs {
            let clause_label = get_label();
            let end_label = get_label();
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    jne {0}\n    jmp {1}\n{0}:\n",
                    clause_label, end_label
                )
                .as_str(),
            );
            res.push_str(boe.to_assembly(var_map)?.as_str());
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                    end_label
                )
                .as_str(),
            );
        }
        Ok(res)
    }
}

impl ToAssembly for BitOrExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.bit_xor_expr.to_assembly(var_map)?;
        for bxe in &self.bit_xor_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bxe.to_assembly(var_map)?.as_str());
            res.push_str("    pop %rcx\n    or %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for BitXorExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.bit_and_expr.to_assembly(var_map)?;
        for bae in &self.bit_and_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bae.to_assembly(var_map)?.as_str());
            res.push_str("    pop %rcx\n    xor %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for BitAndExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.eq_expr.to_assembly(var_map)?;
        for ee in &self.eq_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ee.to_assembly(var_map)?.as_str());
            res.push_str("    pop %rcx\n    and %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for EqExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.rel_expr.to_assembly(var_map)?;
        for (ro, re) in &self.rel_exprs {
            res.push_str("    push %rax\n");
            res.push_str(re.to_assembly(var_map)?.as_str());
            res.push_str(
                format!(
                    "    pop %rcx\n    cmp %rax, %rcx\n    mov $0, %rax\n    set{} %al\n",
                    match ro {
                        EqOp::IsEqual => "e",
                        EqOp::NotEqual => "ne",
                    }
                )
                .as_str(),
            );
        }
        Ok(res)
    }
}

impl ToAssembly for RelExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.shift_expr.to_assembly(var_map)?;
        for (ro, se) in &self.shift_exprs {
            res.push_str("    push %rax\n");
            res.push_str(se.to_assembly(var_map)?.as_str());
            res.push_str(
                format!(
                    "    pop %rcx\n    cmp %rax, %rcx\n    mov $0, %rax\n    set{} %al\n",
                    match ro {
                        RelOp::LT => "l",
                        RelOp::GT => "g",
                        RelOp::LTE => "le",
                        RelOp::GTE => "ge",
                    }
                )
                .as_str(),
            );
        }
        Ok(res)
    }
}

impl ToAssembly for ShiftExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.add_expr.to_assembly(var_map)?;
        for (so, ae) in &self.add_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ae.to_assembly(var_map)?.as_str());
            res.push_str(
                format!(
                    "    mov %rax, %rcx\n    pop %rax\n    sh{} %cl, %rax\n",
                    match so {
                        ShiftOp::Left => "l",
                        ShiftOp::Right => "r",
                    }
                )
                .as_str(),
            );
        }
        Ok(res)
    }
}

impl ToAssembly for AddExpr {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.term.to_assembly(var_map)?;
        for term in &self.terms {
            res.push_str("    push %rax\n");
            match term.0 {
                BinaryOp::Addition => {
                    res.push_str(term.1.to_assembly(var_map)?.as_str());
                    res.push_str("    pop %rcx\n    add %rcx, %rax\n");
                }
                BinaryOp::Subtraction => {
                    res.push_str(term.1.to_assembly(var_map)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    pop %rax\n    sub %rcx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }
}

impl ToAssembly for Term {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        let mut res = self.factor.to_assembly(var_map)?;
        for factor in &self.factors {
            res.push_str("    push %rax\n");
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push_str(factor.1.to_assembly(var_map)?.as_str());
                    res.push_str("    pop %rcx\n    imul %rcx, %rax\n");
                }
                BinaryOp::Divide => {
                    res.push_str(factor.1.to_assembly(var_map)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n");
                }
                BinaryOp::Modulo => {
                    res.push_str(factor.1.to_assembly(var_map)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n    mov %rdx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }
}

impl ToAssembly for Factor {
    fn to_assembly(&self, var_map: &mut HashMap<String, isize>) -> Result<String, String> {
        match self {
            Factor::Expr(expr) => expr.to_assembly(var_map),
            Factor::UnaryOp(uo, factor) => {
                let mut res = factor.to_assembly(var_map)?;
                match uo {
                    UnaryOp::Negation => res.push_str("    neg %rax\n"),
                    UnaryOp::Complement => res.push_str("    not %rax\n"),
                    UnaryOp::Not => {
                        res.push_str("    cmp $0, %rax\n    mov $0, %rax\n    sete %al\n")
                    }
                }
                Ok(res)
            }
            Factor::Constant(i) => Ok(format!("    mov ${}, %rax\n", i)),
            Factor::Identifier(id) => {
                if let Some(offset) = var_map.get(id) {
                    Ok(format!("    mov {}(%rbp), %rax\n", offset))
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
        }
    }
}

pub fn generate(tree: &Program) -> Result<String, String> {
    let mut var_map = HashMap::new();
    tree.to_assembly(&mut var_map)
}

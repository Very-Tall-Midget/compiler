use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use super::ast::*;

fn get_label() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("_LABEL_{}", COUNTER.fetch_add(1, Ordering::Relaxed))
}

trait ToAssembly {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String>;
}

impl ToAssembly for Program {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = String::from("    .globl main\n");
        res.push_str(self.func.to_assembly(var_map, scope, stack_index)?.as_str());
        Ok(res)
    }
}

impl ToAssembly for Function {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = format!("{}:\n    push %rbp\n    mov %rsp, %rbp\n", self.name);
        *stack_index = -8;
        res.push_str(self.body.to_assembly(var_map, scope, stack_index)?.as_str());
        
        if self.name == "main" {
            // placed to return incase a return didn't happen
            res.push_str("    mov $0, %rax\n    mov %rbp, %rsp\n    pop %rbp\n    ret\n");
        }
        
        Ok(res)
    }
}

impl ToAssembly for Statement {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        match self {
            Statement::Return(expr) => {
                let mut res = expr.to_assembly(var_map, scope, stack_index)?;
                res.push_str("    mov %rbp, %rsp\n    pop %rbp\n    ret\n");
                Ok(res)
            }
            Statement::Expr(expr) => expr.to_assembly(var_map, scope, stack_index),
            Statement::If(expr1, statement1, statement2_opt) => {
                let mut res = expr1.to_assembly(var_map, scope, stack_index)?;
                if let Some(statement2) = statement2_opt {
                    let int = get_label();
                    let end = get_label();
                    res.push_str(format!("    cmp $0, %rax\n    je {}\n", int).as_str());
                    res.push_str(statement1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str(format!("    jmp {}\n{}:\n", end, int).as_str());
                    res.push_str(statement2.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str(format!("{}:\n", end).as_str());
                } else {
                    let end = get_label();
                    res.push_str(format!("    cmp $0, %rax\n    je {}\n", end).as_str());
                    res.push_str(statement1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str(format!("{}:\n", end).as_str());
                }

                Ok(res)
            }
            Statement::Block(items) => {
                items.to_assembly(var_map, scope, stack_index)
            }
        }
    }
}

impl ToAssembly for Block {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        _scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = String::new();
        let mut new_scope = HashSet::new();
        let mut new_var_map = var_map.clone();
        let mut new_stack_index = stack_index.clone();
        let mut ret = false;
        for item in &self.items {
            res.push_str(match item {
                BlockItem::Statement(s) => {
                    if let Statement::Return(_) = s {
                        ret = true;
                    }
                    let mut temp_var_map = new_var_map.clone();
                    let mut temp_stack_index = new_stack_index.clone();
                    s.to_assembly(&mut temp_var_map, &mut new_scope, &mut temp_stack_index)?
                }
                BlockItem::Declaration(d) => d.to_assembly(&mut new_var_map, &mut new_scope, &mut new_stack_index)?,
            }.as_str());
        }

        if !ret {
            let bytes_to_deallocate = 8 * new_scope.len() as isize;
            res.push_str(format!("    add ${}, %rsp\n", bytes_to_deallocate).as_str());
        }

        Ok(res)
    }
}

impl ToAssembly for Declarations {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = String::new();
        for (id, expr_opt) in &self.declarations {
            if scope.contains(id) {
                return Err(format!(
                    "[Code Generation]: Variable '{}' already exists",
                    id
                ));
            } else {
                scope.insert(id.clone());
                var_map.insert(id.clone(), *stack_index);
                if let Some(expr) = expr_opt {
                    res.push_str(expr.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    push %rax\n");
                } else {
                    res.push_str("    push $0\n");
                }
                *stack_index -= 8;
            }
        }
        Ok(res)
    }
}

impl ToAssembly for Expr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        match self {
            Expr::Assignment(id, assign_op, expr) => {
                let mut res = expr.to_assembly(var_map, scope, stack_index)?;
                if let Some(offset) = var_map.get(id) {
                    match assign_op {
                        AssignmentOp::Assign => {}
                        AssignmentOp::AddAssign => res.push_str(format!("    mov {0}(%rbp), %rcx\n    add %rcx, %rax\n", offset).as_str()),
                        AssignmentOp::SubAssign => res.push_str(format!("    mov %rax, %rcx\n    mov {0}(%rbp), %rax\n    sub %rcx, %rax\n", offset).as_str()),
                        AssignmentOp::MultAssign => res.push_str(format!("    mov {0}(%rbp), %rcx\n    imul %rcx, %rax\n", offset).as_str()),
                        AssignmentOp::DivAssign => res.push_str(format!("    mov %rax, %rcx\n    mov {0}(%rbp), %rax\n    cqo\n    idiv %rcx\n", offset).as_str()),
                        AssignmentOp::ModAssign => res.push_str(format!("    mov %rax, %rcx\n    mov {0}(%rbp), %rax\n    cqo\n    idiv %rcx\n    mov %rdx, %rax\n", offset).as_str()),
                        AssignmentOp::SLAssign => res.push_str(format!("    mov %rax, %rcx\n    mov {0}(%rbp), %rax\n    shl %cl, %rax\n", offset).as_str()),
                        AssignmentOp::SRAssign => res.push_str(format!("    mov %rax, %rcx\n    mov {0}(%rbp), %rax\n    shr %cl, %rax\n", offset).as_str()),
                        AssignmentOp::BAndAssign => res.push_str(format!("    mov {0}(%rbp), %rcx\n    and %rcx, %rax\n", offset).as_str()),
                        AssignmentOp::BXorAssign => res.push_str(format!("    mov {0}(%rbp), %rcx\n    xor %rcx, %rax\n", offset).as_str()),
                        AssignmentOp::BOrAssign => res.push_str(format!("    mov {0}(%rbp), %rcx\n    or %rcx, %rax\n", offset).as_str()),
                    }

                    res.push_str(format!("    mov %rax, {}(%rbp)\n", offset).as_str());
                    Ok(res)
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Expr::ConditionalExpr(ce) => ce.to_assembly(var_map, scope, stack_index),
        }
    }
}

impl ToAssembly for ConditionalExpr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.log_or_expr.to_assembly(var_map, scope, stack_index)?;
        if let Some((expr1, expr2)) = &self.options {
            let int = get_label();
            let end = get_label();
            res.push_str(format!("    cmp $0, %rax\n    je {}\n", int).as_str());
            res.push_str(expr1.to_assembly(var_map, scope, stack_index)?.as_str());
            res.push_str(format!("    jmp {}\n{}:\n", end, int).as_str());
            res.push_str(expr2.to_assembly(var_map, scope, stack_index)?.as_str());
            res.push_str(format!("{}:\n", end).as_str());
        }

        Ok(res)
    }
}

impl ToAssembly for LogOrExpr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.log_and_expr.to_assembly(var_map, scope, stack_index)?;
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
            res.push_str(lae.to_assembly(var_map, scope, stack_index)?.as_str());
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
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.bit_or_expr.to_assembly(var_map, scope, stack_index)?;
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
            res.push_str(boe.to_assembly(var_map, scope, stack_index)?.as_str());
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
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.bit_xor_expr.to_assembly(var_map, scope, stack_index)?;
        for bxe in &self.bit_xor_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bxe.to_assembly(var_map, scope, stack_index)?.as_str());
            res.push_str("    pop %rcx\n    or %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for BitXorExpr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.bit_and_expr.to_assembly(var_map, scope, stack_index)?;
        for bae in &self.bit_and_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bae.to_assembly(var_map, scope, stack_index)?.as_str());
            res.push_str("    pop %rcx\n    xor %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for BitAndExpr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.eq_expr.to_assembly(var_map, scope, stack_index)?;
        for ee in &self.eq_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ee.to_assembly(var_map, scope, stack_index)?.as_str());
            res.push_str("    pop %rcx\n    and %rcx, %rax\n");
        }
        Ok(res)
    }
}

impl ToAssembly for EqExpr {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.rel_expr.to_assembly(var_map, scope, stack_index)?;
        for (ro, re) in &self.rel_exprs {
            res.push_str("    push %rax\n");
            res.push_str(re.to_assembly(var_map, scope, stack_index)?.as_str());
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
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.shift_expr.to_assembly(var_map, scope, stack_index)?;
        for (ro, se) in &self.shift_exprs {
            res.push_str("    push %rax\n");
            res.push_str(se.to_assembly(var_map, scope, stack_index)?.as_str());
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
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.add_expr.to_assembly(var_map, scope, stack_index)?;
        for (so, ae) in &self.add_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ae.to_assembly(var_map, scope, stack_index)?.as_str());
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
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.term.to_assembly(var_map, scope, stack_index)?;
        for term in &self.terms {
            res.push_str("    push %rax\n");
            match term.0 {
                BinaryOp::Addition => {
                    res.push_str(term.1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    pop %rcx\n    add %rcx, %rax\n");
                }
                BinaryOp::Subtraction => {
                    res.push_str(term.1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    pop %rax\n    sub %rcx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }
}

impl ToAssembly for Term {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        let mut res = self.factor.to_assembly(var_map, scope, stack_index)?;
        for factor in &self.factors {
            res.push_str("    push %rax\n");
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push_str(factor.1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    pop %rcx\n    imul %rcx, %rax\n");
                }
                BinaryOp::Divide => {
                    res.push_str(factor.1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n");
                }
                BinaryOp::Modulo => {
                    res.push_str(factor.1.to_assembly(var_map, scope, stack_index)?.as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n    mov %rdx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }
}

impl ToAssembly for Factor {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        scope: &mut HashSet<String>,
        stack_index: &mut i32,
    ) -> Result<String, String> {
        match self {
            Factor::Expr(expr) => expr.to_assembly(var_map, scope, stack_index),
            Factor::UnaryOp(uo, factor) => {
                let mut res = factor.to_assembly(var_map, scope, stack_index)?;
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
            Factor::Identifier(postfix_id) => postfix_id.to_assembly(var_map, scope, stack_index),
            Factor::Prefix(inc_dec, id) => {
                if let Some(offset) = var_map.get(id) {
                    Ok(format!(
                        "    {1} {0}(%rbp)\n    mov {0}(%rbp), %rax\n",
                        offset,
                        match inc_dec {
                            IncDec::Incremenet => "incq",
                            IncDec::Decrement => "decq",
                        }
                    ))
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
        }
    }
}

impl ToAssembly for PostfixID {
    fn to_assembly(
        &self,
        var_map: &mut HashMap<String, i32>,
        _scope: &mut HashSet<String>,
        _stack_index: &mut i32,
    ) -> Result<String, String> {
        if let Some(offset) = var_map.get(&self.id) {
            if let Some(p) = &self.postfix {
                Ok(format!(
                    "    mov {0}(%rbp), %rax\n    {1} {0}(%rbp)\n",
                    offset,
                    match p {
                        IncDec::Incremenet => "incq",
                        IncDec::Decrement => "decq",
                    }
                ))
            } else {
                Ok(format!("    mov {}(%rbp), %rax\n", offset))
            }
        } else {
            Err(format!(
                "[Code Generation]: Undeclared variable '{}'",
                self.id
            ))
        }
    }
}

pub fn generate(tree: &Program) -> Result<String, String> {
    let mut var_map = HashMap::new();
    let mut scope = HashSet::new();
    let mut stack_index = -8;
    tree.to_assembly(&mut var_map, &mut scope, &mut stack_index)
}

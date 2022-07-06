use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering}
};

use super::ast::*;

type Assembly = String;

pub fn generate(tree: &Program) -> Result<Assembly, String> {
    let mut ctx = Context {
        var_map: HashMap::new(),
        func_map: HashMap::new(),
        scope: HashSet::new(),
        stack_index: -8,
        continue_label: None,
        break_label: None,
    };
    tree.to_assembly(&mut ctx)
}

fn get_label() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("_LABEL_{}", COUNTER.fetch_add(1, Ordering::Relaxed))
}

#[derive(Debug, Clone)]
struct Context {
    pub var_map: HashMap<String, String>, // identifier, location
    pub func_map: HashMap<String, (i32, bool, CallingConv)>, // identifier, (number of parameters, is defined, calling convention)
    pub scope: HashSet<String>,
    pub stack_index: i32,
    pub break_label: Option<String>,
    pub continue_label: Option<String>,
}

trait ToAssembly {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String>;
    fn evaluate(&self) -> Result<i32, String>;
}

impl ToAssembly for Program {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = Assembly::new();
        let mut global_map: HashMap<String, bool> = HashMap::new();
        for item in &self.items {
            match item {
                ProgramItems::Declarations(declerations) => {
                    for (id, expr) in &declerations.declarations {
                        if ctx.func_map.contains_key(id) {
                            return Err(format!("[Code Generation]: Global variable '{}' already defined as a function", id));
                        }
                        ctx.var_map.insert(id.clone(), format!("{}(%rip)", id));
                        if let Some(expr) = expr {
                            if *global_map.get(id).unwrap_or(&false) {
                                return Err(format!(
                                    "[Code Generation]: '{}' defined more than once",
                                    id
                                ));
                            } else {
                                res.push_str(&format!("    .globl {0}\n    .data\n    .align 4\n{0}:\n    .long {1}\n", id, expr.evaluate()?));
                                global_map.insert(id.clone(), true);
                            }
                        } else {
                            global_map.insert(id.clone(), false);
                        }
                    }
                }
                ProgramItems::Function(func) => {
                    if global_map.contains_key(&func.name) {
                        return Err(format!(
                            "[Code Generation]: '{}' is already defined as a global variable",
                            func.name
                        ));
                    }
    
                    if let Some(_) = func.body {
                        res.push_str(&format!("    .globl {}\n    .text\n", func.name));
                        if let Some(info) = ctx.func_map.get(&func.name) {
                            if info.1 {
                                return Err(format!(
                                    "[Code Generation]: Function '{}' is already defined",
                                    func.name
                                ));
                            }
                            if info.0 != func.params.len() as i32 {
                                return Err(format!(
                                    "[Code Generation]: Definition of '{}' disagrees with its declaration",
                                    func.name
                                ));
                            }
                        }
                        ctx.func_map.insert(
                            func.name.clone(),
                            (func.params.len() as i32, true, CallingConv::Cdecl),
                        );
                        let mut ctx = Context {
                            var_map: ctx.var_map.clone(),
                            func_map: ctx.func_map.clone(),
                            scope: HashSet::new(),
                            stack_index: -8,
                            break_label: None,
                            continue_label: None,
                        };
                        let mut param_offset = 16;
                        for param in &func.params {
                            ctx.var_map
                                .insert(param.clone(), format!("{}(%rbp)", param_offset));
                            ctx.scope.insert(param.clone());
                            param_offset += 8;
                        }
                        res.push_str(&func.to_assembly(&mut ctx)?);
                    } else {
                        if let Some(info) = ctx.func_map.get(&func.name) {
                            if info.0 != func.params.len() as i32 {
                                return Err(format!(
                                    "[Code Generation]: Conflicting declarations of '{}'",
                                    func.name
                                ));
                            }
                        } else {
                            ctx.func_map.insert(
                                func.name.clone(),
                                (func.params.len() as i32, false, func.call_conv.clone()),
                            );
                        }
                    }
                }
            }
        }
        for item in global_map {
            if !item.1 {
                res.push_str(&format!("    .globl {0}\n    .bss\n    .align 4\n{0}:\n    .zero 4\n", item.0));
            }
        }

        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate a program".to_string())
    }
}

impl ToAssembly for Function {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = format!(
            "{0}:\n    push %rbp\n    mov %rsp, %rbp\n",
            self.name
        );
        ctx.stack_index = -8;
        res.push_str(
            &self
                .body
                .as_ref()
                .expect("[Code Generation]: lol idk how it managed to get here")
                .to_assembly(ctx)?,
        );

        // placed to return incase a return didn't happen
        res.push_str("    mov $0, %rax\n    mov %rbp, %rsp\n    pop %rbp\n    ret\n");

        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate a function".to_string())
    }
}

impl ToAssembly for Statement {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        match self {
            Statement::Return(expr) => {
                let mut res = expr.to_assembly(ctx)?;
                res.push_str("    mov %rbp, %rsp\n    pop %rbp\n    ret\n");
                Ok(res)
            }
            Statement::Expr(expr) => {
                if let Some(expr) = &expr.expr {
                    expr.to_assembly(ctx)
                } else {
                    Ok(Assembly::new())
                }
            }
            Statement::If(expr1, statement1, statement2_opt) => {
                let mut res = expr1.to_assembly(ctx)?;
                if let Some(statement2) = statement2_opt {
                    let int = get_label();
                    let end = get_label();
                    res.push_str(&format!("    cmp $0, %rax\n    je {}\n", int));
                    res.push_str(&statement1.to_assembly(ctx)?);
                    res.push_str(&format!("    jmp {}\n{}:\n", end, int));
                    res.push_str(&statement2.to_assembly(ctx)?);
                    res.push_str(&format!("{}:\n", end));
                } else {
                    let end = get_label();
                    res.push_str(&format!("    cmp $0, %rax\n    je {}\n", end));
                    res.push_str(&statement1.to_assembly(ctx)?);
                    res.push_str(&format!("{}:\n", end));
                }

                Ok(res)
            }
            Statement::Block(items) => items.to_assembly(ctx),
            Statement::For(init, condition, post_expr, statement) => {
                let mut res = Assembly::new();
                let mut new_ctx = ctx.clone();
                let start = get_label();
                let post_expr_label = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(post_expr_label.clone());

                res.push_str(&init.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("{}:\n", start));
                if let Some(condition) = &condition.expr {
                    res.push_str(&condition.to_assembly(&mut new_ctx)?);
                } else {
                    res.push_str("    mov $1, %rax\n");
                }
                res.push_str(&format!("    cmp $0, %rax\n    je {}\n", end));
                res.push_str(&statement.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("{}:\n", post_expr_label));
                res.push_str(&post_expr.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("    jmp {}\n{}:\n", start, end));

                Ok(res)
            }
            Statement::ForDecl(declerations, condition, post_expr, statement) => {
                let mut res = Assembly::new();
                let mut new_ctx = ctx.clone();
                new_ctx.scope = HashSet::new();
                let start = get_label();
                let post_expr_label = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(post_expr_label.clone());

                res.push_str(&declerations.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("{}:\n", start));
                if let Some(condition) = &condition.expr {
                    res.push_str(&condition.to_assembly(&mut new_ctx)?);
                } else {
                    res.push_str("    mov $1, %rax\n");
                }
                res.push_str(&format!("    cmp $0, %rax\n    je {}\n", end));
                res.push_str(&statement.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("{}:\n", post_expr_label));
                res.push_str(&post_expr.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("    jmp {}\n{}:\n", start, end));

                let bytes_to_deallocate = 8 * new_ctx.scope.len();
                res.push_str(&format!("    add ${}, %rsp\n", bytes_to_deallocate));

                Ok(res)
            }
            Statement::While(expr, statement) => {
                let mut new_ctx = ctx.clone();
                let start = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(start.clone());

                let mut res = format!("{}:\n", start);
                res.push_str(&expr.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("    cmp $0, %rax\n    je {}\n", end));
                res.push_str(&statement.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("    jmp {}\n{}:\n", start, end));

                Ok(res)
            }
            Statement::Do(expr, statement) => {
                let mut new_ctx = ctx.clone();
                let start = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(start.clone());

                let mut res = format!("{}:\n", start);
                res.push_str(&statement.to_assembly(&mut new_ctx)?);
                res.push_str(&expr.to_assembly(&mut new_ctx)?);
                res.push_str(&format!("    cmp $0, %rax\n    jne {}\n{}:\n", start, end));

                Ok(res)
            }
            Statement::Break => {
                if let Some(label) = &ctx.break_label {
                    Ok(format!("    jmp {}\n", label))
                } else {
                    Err("[Code Generation]: Break statement not in loop".to_string())
                }
            }
            Statement::Continue => {
                if let Some(label) = &ctx.continue_label {
                    Ok(format!("    jmp {}\n", label))
                } else {
                    Err("[Code Generation]: Continue statement not in loop".to_string())
                }
            }
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        if let Statement::Expr(expr) = self {
            expr.evaluate()
        } else {
            Err("[Code Generation]: Expected constant value".to_string())
        }
    }
}

impl ToAssembly for Block {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = Assembly::new();
        let mut new_ctx = ctx.clone();
        new_ctx.scope = HashSet::new();
        let mut ret = false;
        for item in &self.items {
            res.push_str(&match item {
                BlockItem::Statement(s) => {
                    if let Statement::Return(_) = s {
                        ret = true;
                    }
                    let mut temp_ctx = new_ctx.clone();
                    let r = s.to_assembly(&mut temp_ctx)?;
                    new_ctx.scope = temp_ctx.scope;
                    r
                }
                BlockItem::Declaration(d) => d.to_assembly(&mut new_ctx)?,
            });
        }

        if !ret {
            let bytes_to_deallocate = 8 * new_ctx.scope.len() as isize;
            res.push_str(&format!("    add ${}, %rsp\n", bytes_to_deallocate));
        }

        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate a block".to_string())
    }
}

impl ToAssembly for Declarations {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = Assembly::new();
        for (id, expr_opt) in &self.declarations {
            if ctx.scope.contains(id) {
                return Err(format!(
                    "[Code Generation]: Variable '{}' already exists",
                    id
                ));
            } else {
                ctx.scope.insert(id.clone());
                ctx.var_map
                    .insert(id.clone(), format!("{}(%rbp)", ctx.stack_index));
                if let Some(expr) = expr_opt {
                    res.push_str(&expr.to_assembly(ctx)?);
                    res.push_str("    push %rax\n");
                } else {
                    res.push_str("    push \n");
                }
                ctx.stack_index -= 8;
            }
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate declerations".to_string())
    }
}

impl ToAssembly for ExprOpt {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        if let Some(expr) = &self.expr {
            expr.to_assembly(ctx)
        } else {
            Ok(Assembly::new())
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        if let Some(expr) = &self.expr {
            expr.evaluate()
        } else {
            Err("[Code Generation]: Cannot evaluate blank expression".to_string())
        }
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        match self {
            Expr::Assignment(id, assign_op, expr) => {
                let mut res = expr.to_assembly(ctx)?;
                if let Some(loc) = ctx.var_map.get(id) {
                    match assign_op {
                        AssignmentOp::Assign => {}
                        AssignmentOp::AddAssign => res.push_str(&format!("    mov {}, %rcx\n    add %rcx, %rax\n", loc)),
                        AssignmentOp::SubAssign => res.push_str(&format!("    mov %rax, %rcx\n    mov {}, %rax\n    sub %rcx, %rax\n", loc)),
                        AssignmentOp::MultAssign => res.push_str(&format!("    mov {}, %rcx\n    imul %rcx, %rax\n", loc)),
                        AssignmentOp::DivAssign => res.push_str(&format!("    mov %rax, %rcx\n    mov {}, %rax\n    cqo\n    idiv %rcx\n", loc)),
                        AssignmentOp::ModAssign => res.push_str(&format!("    mov %rax, %rcx\n    mov {}, %rax\n    cqo\n    idiv %rcx\n    mov %rdx, %rax\n", loc)),
                        AssignmentOp::SLAssign => res.push_str(&format!("    mov %rax, %rcx\n    mov {}, %rax\n    shl %cl, %rax\n", loc)),
                        AssignmentOp::SRAssign => res.push_str(&format!("    mov %rax, %rcx\n    mov {}, %rax\n    shr %cl, %rax\n", loc)),
                        AssignmentOp::BAndAssign => res.push_str(&format!("    mov {}, %rcx\n    and %rcx, %rax\n", loc)),
                        AssignmentOp::BXorAssign => res.push_str(&format!("    mov {}, %rcx\n    xor %rcx, %rax\n", loc)),
                        AssignmentOp::BOrAssign => res.push_str(&format!("    mov {}, %rcx\n    or %rcx, %rax\n", loc)),
                    }

                    res.push_str(&format!("    mov %rax, {}\n", loc));
                    Ok(res)
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Expr::ConditionalExpr(ce) => {
                if let Ok(val) = self.evaluate() {
                    Ok(format!("    mov ${}, %rax\n", val))
                } else {
                    ce.to_assembly(ctx)
                }
            }
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        if let Expr::ConditionalExpr(ce) = self {
            ce.evaluate()
        } else {
            Err("[Code Generation]: Cannot evaluate assignment".to_string())
        }
    }
}

impl ToAssembly for ConditionalExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.log_or_expr.to_assembly(ctx)?;
        if let Some((expr1, expr2)) = &self.options {
            let int = get_label();
            let end = get_label();
            res.push_str(&format!("    cmp $0, %rax\n    je {}\n", int));
            res.push_str(&expr1.to_assembly(ctx)?);
            res.push_str(&format!("    jmp {}\n{}:\n", end, int));
            res.push_str(&expr2.to_assembly(ctx)?);
            res.push_str(&format!("{}:\n", end));
        }

        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        if let Some((expr1, expr2)) = &self.options {
            if self.log_or_expr.evaluate()? != 0 {
                expr1.evaluate()
            } else {
                expr2.evaluate()
            }
        } else {
            self.log_or_expr.evaluate()
        }
    }
}

impl ToAssembly for LogOrExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.log_and_expr.to_assembly(ctx)?;
        for lae in &self.log_and_exprs {
            let clause_label = get_label();
            let end_label = get_label();
            res.push_str(&format!(
                "    cmp $0, %rax\n    je {0}\n    mov $1, %rax\n    jmp {1}\n{0}:\n",
                clause_label, end_label
            ));
            res.push_str(&lae.to_assembly(ctx)?);
            res.push_str(&format!(
                "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                end_label
            ));
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.log_and_expr.evaluate()?;
        for lae in &self.log_and_exprs {
            res = if (res != 0) || (lae.evaluate()? != 0) {
                1
            } else {
                0
            };
        }
        Ok(res)
    }
}

impl ToAssembly for LogAndExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.bit_or_expr.to_assembly(ctx)?;
        for boe in &self.bit_or_exprs {
            let clause_label = get_label();
            let end_label = get_label();
            res.push_str(&format!(
                "    cmp $0, %rax\n    jne {0}\n    jmp {1}\n{0}:\n",
                clause_label, end_label
            ));
            res.push_str(&boe.to_assembly(ctx)?);
            res.push_str(&format!(
                "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                end_label
            ));
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.bit_or_expr.evaluate()?;
        for boe in &self.bit_or_exprs {
            res = if (res != 0) && (boe.evaluate()? != 0) {
                1
            } else {
                0
            };
        }
        Ok(res)
    }
}

impl ToAssembly for BitOrExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.bit_xor_expr.to_assembly(ctx)?;
        for bxe in &self.bit_xor_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&bxe.to_assembly(ctx)?);
            res.push_str("    pop %rcx\n    or %rcx, %rax\n");
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.bit_xor_expr.evaluate()?;
        for bxe in &self.bit_xor_exprs {
            res |= bxe.evaluate()?;
        }
        Ok(res)
    }
}

impl ToAssembly for BitXorExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.bit_and_expr.to_assembly(ctx)?;
        for bae in &self.bit_and_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&bae.to_assembly(ctx)?);
            res.push_str("    pop %rcx\n    xor %rcx, %rax\n");
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.bit_and_expr.evaluate()?;
        for bae in &self.bit_and_exprs {
            res ^= bae.evaluate()?;
        }
        Ok(res)
    }
}

impl ToAssembly for BitAndExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.eq_expr.to_assembly(ctx)?;
        for ee in &self.eq_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&ee.to_assembly(ctx)?);
            res.push_str("    pop %rcx\n    and %rcx, %rax\n");
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.eq_expr.evaluate()?;
        for ee in &self.eq_exprs {
            res &= ee.evaluate()?;
        }
        Ok(res)
    }
}

impl ToAssembly for EqExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.rel_expr.to_assembly(ctx)?;
        for (ro, re) in &self.rel_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&re.to_assembly(ctx)?);
            res.push_str(&format!(
                "    pop %rcx\n    cmp %rax, %rcx\n    mov $0, %rax\n    set{} %al\n",
                match ro {
                    EqOp::IsEqual => "e",
                    EqOp::NotEqual => "ne",
                }
            ));
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.rel_expr.evaluate()?;
        for (eo, re) in &self.rel_exprs {
            res = match eo {
                EqOp::IsEqual => if re.evaluate()? == res { 1 } else { 0 },
                EqOp::NotEqual => if re.evaluate()? != res { 1 } else { 0 },
            };
        }
        Ok(res)
    }
}

impl ToAssembly for RelExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.shift_expr.to_assembly(ctx)?;
        for (ro, se) in &self.shift_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&se.to_assembly(ctx)?);
            res.push_str(&format!(
                "    pop %rcx\n    cmp %rax, %rcx\n    mov $0, %rax\n    set{} %al\n",
                match ro {
                    RelOp::LT => "l",
                    RelOp::GT => "g",
                    RelOp::LTE => "le",
                    RelOp::GTE => "ge",
                }
            ));
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.shift_expr.evaluate()?;
        for (ro, se) in &self.shift_exprs {
            res = match ro {
                RelOp::LT => if res < se.evaluate()? { 1 } else { 0 },
                RelOp::LTE => if res <= se.evaluate()? { 1 } else { 0 },
                RelOp::GT => if res > se.evaluate()? { 1 } else { 0 },
                RelOp::GTE => if res >= se.evaluate()? { 1 } else { 0 },
            };
        }
        Ok(res)
    }
}

impl ToAssembly for ShiftExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.add_expr.to_assembly(ctx)?;
        for (so, ae) in &self.add_exprs {
            res.push_str("    push %rax\n");
            res.push_str(&ae.to_assembly(ctx)?);
            res.push_str(&format!(
                "    mov %rax, %rcx\n    pop %rax\n    sh{} %cl, %rax\n",
                match so {
                    ShiftOp::Left => "l",
                    ShiftOp::Right => "r",
                }
            ));
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.add_expr.evaluate()?;
        for (so, ae) in &self.add_exprs {
            res = match so {
                ShiftOp::Left => res << ae.evaluate()?,
                ShiftOp::Right => res >> ae.evaluate()?,
            };
        }
        Ok(res)
    }
}

impl ToAssembly for AddExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.term.to_assembly(ctx)?;
        for term in &self.terms {
            res.push_str("    push %rax\n");
            match term.0 {
                BinaryOp::Addition => {
                    res.push_str(&term.1.to_assembly(ctx)?);
                    res.push_str("    pop %rcx\n    add %rcx, %rax\n");
                }
                BinaryOp::Subtraction => {
                    res.push_str(&term.1.to_assembly(ctx)?);
                    res.push_str("    mov %rax, %rcx\n    pop %rax\n    sub %rcx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.term.evaluate()?;
        for (bo, term) in &self.terms {
            res = match bo {
                BinaryOp::Addition => res + term.evaluate()?,
                BinaryOp::Subtraction => res - term.evaluate()?,
                _ => unreachable!(),
            };
        }
        Ok(res)
    }
}

impl ToAssembly for Term {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.factor.to_assembly(ctx)?;
        for factor in &self.factors {
            res.push_str("    push %rax\n");
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push_str(&factor.1.to_assembly(ctx)?);
                    res.push_str("    pop %rcx\n    imul %rcx, %rax\n");
                }
                BinaryOp::Divide => {
                    res.push_str(&factor.1.to_assembly(ctx)?);
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n");
                }
                BinaryOp::Modulo => {
                    res.push_str(&factor.1.to_assembly(ctx)?);
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n    mov %rdx, %rax\n");
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.factor.evaluate()?;
        for (bo, factor) in &self.factors {
            res = match bo {
                BinaryOp::Multiply => res * factor.evaluate()?,
                BinaryOp::Divide => res / factor.evaluate()?,
                BinaryOp::Modulo => res % factor.evaluate()?,
                _ => unreachable!(),
            };
        }
        Ok(res)
    }
}

impl ToAssembly for Factor {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        match self {
            Factor::Expr(expr) => expr.to_assembly(ctx),
            Factor::UnaryOp(uo, factor) => {
                let mut res = factor.to_assembly(ctx)?;
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
            Factor::Identifier(postfix_id) => postfix_id.to_assembly(ctx),
            Factor::Prefix(inc_dec, id) => {
                if let Some(loc) = ctx.var_map.get(id) {
                    Ok(format!(
                        "    {1} {0}\n    mov {0}, %rax\n",
                        loc,
                        match inc_dec {
                            IncDec::Incremenet => "incq",
                            IncDec::Decrement => "decq",
                        }
                    ))
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Factor::FunctionCall(id, args) => {
                if let Some(func) = ctx.func_map.get(id) {
                    if func.0 != args.len() as i32 {
                        Err(format!(
                            "[Code Generation]: Incorrect number of arguments for '{}'",
                            id
                        ))
                    } else {
                        let mut res = Assembly::new();
                        match func.2 {
                            CallingConv::Cdecl => {
                                for arg in args.iter().rev() {
                                    res.push_str(&arg.to_assembly(ctx)?);
                                    res.push_str("    push %rax\n");
                                }
                                res.push_str(&format!("    call {}\n", id));
                                res.push_str(&format!("    add ${}, %rsp\n", args.len() * 8));
                            }
                            CallingConv::Syscall => {
                                if args.len() > 3 {
                                    return Err("[Code Generation]: __syscall calls with more than 3 arguments are not supported".to_string());
                                }
                                if let Some(arg1) = args.get(0) {
                                    res.push_str(&arg1.to_assembly(ctx)?);
                                    res.push_str("    mov %rax, %rdi\n");
                                    if let Some(arg2) = args.get(1) {
                                        res.push_str(&arg2.to_assembly(ctx)?);
                                        res.push_str("    mov %rax, %rsi\n");
                                        if let Some(arg3) = args.get(2) {
                                            res.push_str(&arg3.to_assembly(ctx)?);
                                            res.push_str("    mov %rax, %rdx\n");
                                        }
                                    }
                                }
                                res.push_str(&format!("    call {}\n", id));
                            }
                        }

                        Ok(res)
                    }
                } else {
                    Err(format!("[Code Generation]: Unknown function '{}'", id))
                }
            }
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        match self {
            Factor::Expr(expr) => expr.evaluate(),
            Factor::UnaryOp(uo, factor) => {
                match uo {
                    UnaryOp::Negation => Ok(-factor.evaluate()?),
                    UnaryOp::Complement => Ok(!factor.evaluate()?),
                    UnaryOp::Not => Ok(if factor.evaluate()? == 0 { 1 } else { 0 }),
                }
            }
            Factor::Constant(i) => Ok(*i as i32),
            Factor::Identifier(postfix_id) => postfix_id.evaluate(),
            Factor::Prefix(_, _) => Err("[Code Generation]: Cannot evaluate identifiers".to_string()),
            Factor::FunctionCall(_, _) => Err("[Code Generation]: Cannot evaluate a function call".to_string()),
        }
    }
}

impl ToAssembly for PostfixID {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        if let Some(loc) = ctx.var_map.get(&self.id) {
            if let Some(p) = &self.postfix {
                Ok(format!(
                    "    mov {0}, %rax\n    {1} {0}\n",
                    loc,
                    match p {
                        IncDec::Incremenet => "incq",
                        IncDec::Decrement => "decq",
                    }
                ))
            } else {
                Ok(format!("    mov {}, %rax\n", loc))
            }
        } else {
            Err(format!(
                "[Code Generation]: Undeclared variable '{}'",
                self.id
            ))
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate identifiers".to_string())
    }
}

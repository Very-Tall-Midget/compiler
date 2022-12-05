use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use super::assembly::*;
use super::ast::*;

macro_rules! gen_into(
    ($from:ident -> $to:ident: [ $($match_from:ident => $match_to:ident),+ $(,)? ]) => {
        impl From<$from> for $to {
            fn from(val: $from) -> $to {
                match val {
                    $($from::$match_from => $to::$match_to),+,
                }
            }
        }
    };
);

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
    format!("LABEL_{}", COUNTER.fetch_add(1, Ordering::Relaxed))
}

#[derive(Debug, Clone)]
pub struct Context {
    pub var_map: HashMap<String, String>, // identifier, location
    pub func_map: HashMap<String, (usize, bool)>, // identifier, (number of parameters, is defined, calling convention)
    pub scope: HashSet<String>,
    pub stack_index: i32,
    pub break_label: Option<String>,
    pub continue_label: Option<String>,
}

#[derive(Debug, Clone)]
pub struct CheckContext {
    pub var_map: HashSet<String>,
    pub func_map: HashMap<String, (usize, bool)>, // identifier, (number of parameters, is defined, calling convention)
    pub scope: HashSet<String>,
    pub break_label: bool,
    pub continue_label: bool,
}

pub trait ToAssembly {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String>;
    fn check(&self, ctx: &mut CheckContext) -> Result<(), String>;
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
                        ctx.var_map.insert(id.clone(), format!("[{}]", id));
                        if let Some(expr) = expr {
                            if *global_map.get(id).unwrap_or(&false) {
                                return Err(format!(
                                    "[Code Generation]: '{}' defined more than once",
                                    id
                                ));
                            } else {
                                res.push(OpCode::GlobalVariable(id.clone(), expr.evaluate()?));
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

                    let mut unique_check = HashSet::new();
                    if !func.params.iter().all(move |x| unique_check.insert(x)) {
                        return Err(format!(
                            "[Code Generation]: Duplicate parameter name in declaration of '{}'",
                            func.name
                        ));
                    }

                    if func.body.is_some() {
                        res.push(OpCode::FunctionDecl(func.name.clone()));
                        if let Some(info) = ctx.func_map.get(&func.name) {
                            if info.1 {
                                return Err(format!(
                                    "[Code Generation]: Function '{}' is already defined",
                                    func.name
                                ));
                            }
                            if info.0 != func.params.len() {
                                return Err(format!(
                                    "[Code Generation]: Definition of '{}' disagrees with its declaration",
                                    func.name
                                ));
                            }
                        }
                        ctx.func_map
                            .insert(func.name.clone(), (func.params.len(), true));
                        let mut ctx = Context {
                            var_map: ctx.var_map.clone(),
                            func_map: ctx.func_map.clone(),
                            scope: HashSet::new(),
                            stack_index: -8,
                            break_label: None,
                            continue_label: None,
                        };

                        let locations = vec![
                            Location::rcx(),
                            Location::rdx(),
                            Location::r8(),
                            Location::r9(),
                        ];
                        for (param, loc) in func.params.iter().take(locations.len()).zip(&locations)
                        {
                            ctx.var_map.insert(param.clone(), loc.name.clone());
                            ctx.scope.insert(param.clone());
                        }
                        if func.params.len() > locations.len() {
                            let mut param_offset = 56;
                            for param in func.params.iter().skip(locations.len()) {
                                ctx.var_map
                                    .insert(param.clone(), format!("[rbp+{}]", param_offset));
                                ctx.scope.insert(param.clone());
                                param_offset += 8;
                            }
                        }

                        res.push_asm(&func.to_assembly(&mut ctx)?);
                    } else if let Some(info) = ctx.func_map.get(&func.name) {
                        if info.0 != func.params.len() {
                            return Err(format!(
                                "[Code Generation]: Conflicting declarations of '{}'",
                                func.name
                            ));
                        }
                    } else {
                        ctx.func_map
                            .insert(func.name.clone(), (func.params.len(), false));
                    }
                }
            }
        }
        for item in global_map {
            if !item.1 {
                res.push(OpCode::GlobalVariable(item.0.clone(), 0));
            }
        }

        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        let mut global_map: HashMap<String, bool> = HashMap::new();
        for item in &self.items {
            match item {
                ProgramItems::Declarations(declerations) => {
                    for (id, expr) in &declerations.declarations {
                        if ctx.func_map.contains_key(id) {
                            return Err(format!("[Code Generation]: Global variable '{}' already defined as a function", id));
                        }
                        ctx.var_map.insert(id.clone());
                        if let Some(expr) = expr {
                            if *global_map.get(id).unwrap_or(&false) {
                                return Err(format!(
                                    "[Code Generation]: '{}' defined more than once",
                                    id
                                ));
                            } else {
                                expr.evaluate().expect("Global vars must be constant"); // Check the value is a constant
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

                    if func.body.is_some() {
                        if let Some(info) = ctx.func_map.get(&func.name) {
                            if info.1 {
                                return Err(format!(
                                    "[Code Generation]: Function '{}' is already defined",
                                    func.name
                                ));
                            }
                            if info.0 != func.params.len() {
                                return Err(format!(
                                    "[Code Generation]: Definition of '{}' disagrees with its declaration",
                                    func.name
                                ));
                            }
                        }
                        ctx.func_map
                            .insert(func.name.clone(), (func.params.len(), true));
                        let mut ctx = CheckContext {
                            var_map: ctx.var_map.clone(),
                            func_map: ctx.func_map.clone(),
                            scope: HashSet::new(),
                            break_label: false,
                            continue_label: false,
                        };
                        for param in &func.params {
                            ctx.var_map.insert(param.clone());
                            ctx.scope.insert(param.clone());
                        }
                        func.check(&mut ctx)?;
                    } else if let Some(info) = ctx.func_map.get(&func.name) {
                        if info.0 != func.params.len() {
                            return Err(format!(
                                "[Code Generation]: Conflicting declarations of '{}'",
                                func.name
                            ));
                        }
                    } else {
                        ctx.func_map
                            .insert(func.name.clone(), (func.params.len(), false));
                    }
                }
            }
        }

        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        Err("[Code Generation]: Cannot evaluate a program".to_string())
    }
}

impl ToAssembly for Function {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = Assembly::new();
        res.push(OpCode::StartFunction(self.name.clone()));
        res.push(OpCode::PreserveStack);
        ctx.stack_index = -8;
        res.push_asm(
            &self
                .body
                .as_ref()
                .expect("[Code Generation]: lol idk how it managed to get here")
                .to_assembly(ctx)?,
        );

        if !matches!(res.last(), Some(OpCode::Return)) {
            // placed to return incase a return didn't happen
            res.push(OpCode::MovImmediate(0, Location::rax()));
            res.push(OpCode::RestoreStack);
            res.push(OpCode::Return);
        };

        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.body.as_ref().unwrap().check(ctx)
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
                res.push(OpCode::RestoreStack);
                res.push(OpCode::Return);
                Ok(res)
            }
            Statement::Expr(expr) => {
                if let Some(expr) = &expr.expr {
                    expr.to_assembly(ctx)
                } else {
                    Ok(Assembly::new())
                }
            }
            Statement::If(expr, statement1, statement2_opt) => {
                if let Ok(val) = expr.evaluate() {
                    if val != 0 {
                        if let Some(statement2) = statement2_opt {
                            statement2.borrow().to_assembly(ctx)
                        } else {
                            Ok(Assembly::new())
                        }
                    } else {
                        statement1.borrow().to_assembly(ctx)
                    }
                } else {
                    let mut res = expr.to_assembly(ctx)?;
                    if let Some(statement2) = statement2_opt {
                        let int = get_label();
                        let end = get_label();
                        res.push(OpCode::CompareImmediate(0, Location::rax()));
                        res.push(OpCode::Jump(JumpCondition::Equal, int.clone()));
                        res.push_asm(&statement1.borrow().to_assembly(ctx)?);
                        res.push(OpCode::Jump(JumpCondition::Unconditional, end.clone()));
                        res.push(OpCode::Label(int));
                        res.push_asm(&statement2.borrow().to_assembly(ctx)?);
                        res.push(OpCode::Label(end));
                    } else {
                        let end = get_label();
                        res.push(OpCode::CompareImmediate(0, Location::rax()));
                        res.push(OpCode::Jump(JumpCondition::Equal, end.clone()));
                        res.push_asm(&statement1.borrow().to_assembly(ctx)?);
                        res.push(OpCode::Label(end));
                    }
                    Ok(res)
                }
            }
            Statement::Block(items) => items.to_assembly(ctx),
            Statement::For(init, condition, post_expr, statement) => {
                if let Some(cond) = &condition.expr {
                    if let Ok(val) = cond.evaluate() {
                        if val == 0 {
                            return Ok(Assembly::new());
                        }
                    }
                }

                let mut res = Assembly::new();
                let mut new_ctx = ctx.clone();
                let start = get_label();
                let post_expr_label = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(post_expr_label.clone());

                res.push_asm(&init.to_assembly(&mut new_ctx)?);
                res.push(OpCode::Label(start.clone()));
                if let Some(condition) = &condition.expr {
                    res.push_asm(&condition.to_assembly(&mut new_ctx)?);
                } else {
                    res.push(OpCode::MovImmediate(1, Location::rax()));
                }
                res.push(OpCode::CompareImmediate(0, Location::rax()));
                res.push(OpCode::Jump(JumpCondition::Equal, end.clone()));
                res.push_asm(&statement.borrow().to_assembly(&mut new_ctx)?);
                res.push(OpCode::Label(post_expr_label));
                res.push_asm(&post_expr.to_assembly(&mut new_ctx)?);
                res.push(OpCode::Jump(JumpCondition::Unconditional, start));
                res.push(OpCode::Label(end));

                Ok(res)
            }
            Statement::ForDecl(declerations, condition, post_expr, statement) => {
                if let Some(cond) = &condition.expr {
                    if let Ok(val) = cond.evaluate() {
                        if val == 0 {
                            return Ok(Assembly::new());
                        }
                    }
                }

                let mut res = Assembly::new();
                let mut new_ctx = ctx.clone();
                new_ctx.scope = HashSet::new();
                let start = get_label();
                let post_expr_label = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(post_expr_label.clone());

                res.push_asm(&declerations.to_assembly(&mut new_ctx)?);
                res.push(OpCode::Label(start.clone()));
                if let Some(condition) = &condition.expr {
                    res.push_asm(&condition.to_assembly(&mut new_ctx)?);
                } else {
                    res.push(OpCode::MovImmediate(1, Location::rax()));
                }
                res.push(OpCode::CompareImmediate(0, Location::rax()));
                res.push(OpCode::Jump(JumpCondition::Equal, end.clone()));
                res.push_asm(&statement.borrow().to_assembly(&mut new_ctx)?);
                res.push(OpCode::Label(post_expr_label));
                res.push_asm(&post_expr.to_assembly(&mut new_ctx)?);
                res.push(OpCode::Jump(JumpCondition::Unconditional, start));
                res.push(OpCode::Label(end));

                let bytes_to_deallocate = 8 * new_ctx.scope.len();
                if bytes_to_deallocate != 0 {
                    res.push(OpCode::AddImmediate(
                        bytes_to_deallocate as i32,
                        Location::rsp(),
                    ));
                }

                Ok(res)
            }
            Statement::While(expr, statement) => {
                if let Ok(val) = expr.evaluate() {
                    if val == 0 {
                        return Ok(Assembly::new());
                    }
                }

                let mut new_ctx = ctx.clone();
                let start = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(start.clone());

                let mut res = Assembly::new();
                res.push(OpCode::Label(start.clone()));
                res.push_asm(&expr.to_assembly(&mut new_ctx)?);
                res.push(OpCode::CompareImmediate(0, Location::rax()));
                res.push(OpCode::Jump(JumpCondition::Equal, end.clone()));
                res.push_asm(&statement.borrow().to_assembly(&mut new_ctx)?);
                res.push(OpCode::Jump(JumpCondition::Unconditional, start));
                res.push(OpCode::Label(end));

                Ok(res)
            }
            Statement::Do(expr, statement) => {
                if let Ok(val) = expr.evaluate() {
                    if val == 0 {
                        let mut new_ctx = ctx.clone();
                        let label = get_label();
                        new_ctx.break_label = Some(label.clone());
                        new_ctx.continue_label = Some(label.clone());

                        let mut res = statement.borrow().to_assembly(&mut new_ctx)?;
                        res.push(OpCode::Label(label));
                        return Ok(res);
                    }
                }

                let mut new_ctx = ctx.clone();
                let start = get_label();
                let end = get_label();
                new_ctx.break_label = Some(end.clone());
                new_ctx.continue_label = Some(start.clone());

                let mut res = Assembly::new();
                res.push(OpCode::Label(start.clone()));
                res.push_asm(&statement.borrow().to_assembly(&mut new_ctx)?);
                res.push_asm(&expr.to_assembly(&mut new_ctx)?);
                res.push(OpCode::CompareImmediate(0, Location::rax()));
                res.push(OpCode::Jump(JumpCondition::NotEqual, start));
                res.push(OpCode::Label(end));

                Ok(res)
            }
            Statement::Break => {
                if let Some(label) = &ctx.break_label {
                    let mut res = Assembly::new();
                    res.push(OpCode::Jump(JumpCondition::Unconditional, label.clone()));
                    Ok(res)
                } else {
                    Err("[Code Generation]: Break statement not in loop".to_string())
                }
            }
            Statement::Continue => {
                if let Some(label) = &ctx.continue_label {
                    let mut res = Assembly::new();
                    res.push(OpCode::Jump(JumpCondition::Unconditional, label.clone()));
                    Ok(res)
                } else {
                    Err("[Code Generation]: Continue statement not in loop".to_string())
                }
            }
        }
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        match self {
            Statement::Return(expr) => expr.check(ctx),
            Statement::Expr(expr) => {
                if let Some(expr) = &expr.expr {
                    expr.check(ctx)
                } else {
                    Ok(())
                }
            }
            Statement::If(expr, statement1, statement2_opt) => {
                expr.check(ctx)?;
                statement1.borrow_mut().check(ctx)?;
                if let Some(statement2) = statement2_opt {
                    statement2.borrow_mut().check(ctx)
                } else {
                    Ok(())
                }
            }
            Statement::Block(items) => items.check(ctx),
            Statement::For(init, condition, post_expr, statement) => {
                let mut new_ctx = ctx.clone();
                new_ctx.break_label = true;
                new_ctx.continue_label = true;

                init.check(&mut new_ctx)?;
                if let Some(condition) = &condition.expr {
                    condition.check(&mut new_ctx)?;
                }
                statement.borrow_mut().check(&mut new_ctx)?;
                post_expr.check(&mut new_ctx)
            }
            Statement::ForDecl(declerations, condition, post_expr, statement) => {
                let mut new_ctx = ctx.clone();
                new_ctx.scope = HashSet::new();
                new_ctx.break_label = true;
                new_ctx.continue_label = true;

                declerations.check(&mut new_ctx)?;
                if let Some(condition) = &condition.expr {
                    condition.check(&mut new_ctx)?;
                }
                statement.borrow_mut().check(&mut new_ctx)?;
                post_expr.check(&mut new_ctx)
            }
            Statement::While(expr, statement) => {
                let mut new_ctx = ctx.clone();
                new_ctx.break_label = true;
                new_ctx.continue_label = true;

                expr.check(&mut new_ctx)?;
                statement.borrow_mut().check(&mut new_ctx)
            }
            Statement::Do(expr, statement) => {
                let mut new_ctx = ctx.clone();
                new_ctx.break_label = true;
                new_ctx.continue_label = true;

                statement.borrow_mut().check(&mut new_ctx)?;
                expr.check(&mut new_ctx)
            }
            Statement::Break => {
                if !ctx.break_label {
                    Err("[Code Generation]: Break statement not in loop".to_string())
                } else {
                    Ok(())
                }
            }
            Statement::Continue => {
                if ctx.continue_label {
                    Err("[Code Generation]: Continue statement not in loop".to_string())
                } else {
                    Ok(())
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
        for item in &self.items {
            res.push_asm(&match item {
                BlockItem::Statement(s) => {
                    let mut temp_ctx = new_ctx.clone();
                    let r = s.to_assembly(&mut temp_ctx)?;
                    new_ctx.scope = temp_ctx.scope;
                    r
                }
                BlockItem::Declaration(d) => d.to_assembly(&mut new_ctx)?,
            });
        }

        let bytes_to_deallocate = 8 * new_ctx.scope.len();
        if bytes_to_deallocate != 0 {
            res.push(OpCode::AddImmediate(
                bytes_to_deallocate as i32,
                Location::rsp(),
            ));
        }

        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        let mut new_ctx = ctx.clone();
        new_ctx.scope = HashSet::new();
        for item in &self.items {
            match item {
                BlockItem::Statement(s) => {
                    let mut temp_ctx = new_ctx.clone();
                    s.check(&mut temp_ctx)?;
                    new_ctx.scope = temp_ctx.scope;
                }
                BlockItem::Declaration(d) => d.check(&mut new_ctx)?,
            }
        }

        Ok(())
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
                    .insert(id.clone(), format!("[rbp+{}]", ctx.stack_index));
                if let Some(expr) = expr_opt {
                    res.push_asm(&expr.to_assembly(ctx)?);
                    res.push(OpCode::Push(Location::rax()));
                } else {
                    res.push(OpCode::PushImmediate(0));
                }
                ctx.stack_index -= 8;
            }
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        for (id, expr_opt) in &self.declarations {
            if ctx.scope.contains(id) {
                return Err(format!(
                    "[Code Generation]: Variable '{}' already exists in this scope",
                    id
                ));
            } else {
                ctx.var_map.insert(id.clone());
                ctx.scope.insert(id.clone());
                if let Some(expr) = expr_opt {
                    expr.check(ctx)?;
                }
            }
        }
        Ok(())
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

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        if let Some(expr) = &self.expr {
            expr.check(ctx)
        } else {
            Ok(())
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
                let mut res = expr.borrow().to_assembly(ctx)?;
                if let Some(loc) = ctx.var_map.get(id) {
                    let loc = Location {
                        name: loc.clone(),
                        size: LocationSize::Qword,
                    };
                    match assign_op {
                        AssignmentOp::Assign => {}
                        AssignmentOp::AddAssign => {
                            res.push(OpCode::Mov(loc.clone(), Location::r10()));
                            res.push(OpCode::Add(Location::r10(), Location::rax()));
                        }
                        AssignmentOp::SubAssign => {
                            res.push(OpCode::Mov(Location::rax(), Location::r10()));
                            res.push(OpCode::Mov(loc.clone(), Location::rax()));
                            res.push(OpCode::Sub(Location::r10(), Location::rax()));
                        }
                        AssignmentOp::MultAssign => {
                            res.push(OpCode::Mov(loc.clone(), Location::r10()));
                            res.push(OpCode::Mult(Location::r10(), Location::rax()));
                        }
                        AssignmentOp::DivAssign => {
                            res.push(OpCode::Mov(Location::rax(), Location::r10()));
                            res.push(OpCode::Mov(loc.clone(), Location::rax()));
                            res.push(OpCode::SignExtend);
                            res.push(OpCode::IDiv(Location::r10()));
                        }
                        AssignmentOp::ModAssign => {
                            res.push(OpCode::Mov(Location::rax(), Location::r10()));
                            res.push(OpCode::Mov(loc.clone(), Location::rax()));
                            res.push(OpCode::SignExtend);
                            res.push(OpCode::IDiv(Location::r10()));
                            res.push(OpCode::Mov(Location::rdx(), Location::rax()));
                        }
                        AssignmentOp::SLAssign => {
                            res.push(OpCode::Mov(Location::rax(), Location::r10()));
                            res.push(OpCode::Mov(loc.clone(), Location::rax()));
                            res.push(OpCode::ShiftLeft(Location::r10l(), Location::rax()));
                        }
                        AssignmentOp::SRAssign => {
                            res.push(OpCode::Mov(Location::rax(), Location::r10()));
                            res.push(OpCode::Mov(loc.clone(), Location::rax()));
                            res.push(OpCode::ShiftRight(Location::r10l(), Location::rax()));
                        }
                        AssignmentOp::BAndAssign => {
                            res.push(OpCode::Mov(loc.clone(), Location::r10()));
                            res.push(OpCode::And(Location::r10(), Location::rax()));
                        }
                        AssignmentOp::BXorAssign => {
                            res.push(OpCode::Mov(loc.clone(), Location::r10()));
                            res.push(OpCode::Xor(Location::r10(), Location::rax()));
                        }
                        AssignmentOp::BOrAssign => {
                            res.push(OpCode::Mov(loc.clone(), Location::r10()));
                            res.push(OpCode::Or(Location::r10(), Location::rax()));
                        }
                    }

                    res.push(OpCode::Mov(Location::rax(), loc));
                    Ok(res)
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Expr::ConditionalExpr(ce) => {
                if let Ok(val) = self.evaluate() {
                    let mut res = Assembly::new();
                    res.push(OpCode::MovImmediate(val, Location::rax()));
                    Ok(res)
                } else {
                    ce.to_assembly(ctx)
                }
            }
        }
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        match self {
            Expr::Assignment(id, _, expr) => {
                expr.borrow_mut().check(ctx)?;
                if ctx.var_map.contains(id) {
                    Ok(())
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Expr::ConditionalExpr(ce) => ce.check(ctx),
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
            res.push(OpCode::CompareImmediate(0, Location::rax()));
            res.push(OpCode::Jump(JumpCondition::Equal, int.clone()));
            res.push_asm(&expr1.borrow().to_assembly(ctx)?);
            res.push(OpCode::Jump(JumpCondition::Unconditional, end.clone()));
            res.push(OpCode::Label(int));
            res.push_asm(&expr2.borrow().to_assembly(ctx)?);
            res.push(OpCode::Label(end));
        }

        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.log_or_expr.check(ctx)?;
        if let Some((expr1, expr2)) = &self.options {
            expr1.borrow_mut().check(ctx)?;
            expr2.borrow_mut().check(ctx)?;
        }

        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        if let Some((expr1, expr2)) = &self.options {
            if self.log_or_expr.evaluate()? != 0 {
                expr1.borrow().evaluate()
            } else {
                expr2.borrow().evaluate()
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
            res.push(OpCode::CompareImmediate(0, Location::rax()));
            res.push(OpCode::Jump(JumpCondition::Equal, clause_label.clone()));
            res.push(OpCode::MovImmediate(1, Location::rax()));
            res.push(OpCode::Jump(
                JumpCondition::Unconditional,
                end_label.clone(),
            ));
            res.push(OpCode::Label(clause_label));
            res.push_asm(&lae.to_assembly(ctx)?);
            res.push(OpCode::CompareImmediate(0, Location::rax()));
            res.push(OpCode::MovImmediate(0, Location::rax()));
            res.push(OpCode::Set(SetCondition::NotEqual, Location::al()));
            res.push(OpCode::Label(end_label));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.log_and_expr.check(ctx)?;
        for lae in &self.log_and_exprs {
            lae.check(ctx)?;
        }
        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.log_and_expr.evaluate()?;
        for lae in &self.log_and_exprs {
            res = i32::from((res != 0) || (lae.evaluate()? != 0));
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
            res.push(OpCode::CompareImmediate(0, Location::rax()));
            res.push(OpCode::Jump(JumpCondition::NotEqual, clause_label.clone()));
            res.push(OpCode::Jump(
                JumpCondition::Unconditional,
                end_label.clone(),
            ));
            res.push(OpCode::Label(clause_label));
            res.push_asm(&boe.to_assembly(ctx)?);
            res.push(OpCode::CompareImmediate(0, Location::rax()));
            res.push(OpCode::MovImmediate(0, Location::rax()));
            res.push(OpCode::Set(SetCondition::NotEqual, Location::al()));
            res.push(OpCode::Label(end_label));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.bit_or_expr.check(ctx)?;
        for boe in &self.bit_or_exprs {
            boe.check(ctx)?;
        }
        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.bit_or_expr.evaluate()?;
        for boe in &self.bit_or_exprs {
            res = i32::from((res != 0) && (boe.evaluate()? != 0));
        }
        Ok(res)
    }
}

impl ToAssembly for BitOrExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.bit_xor_expr.to_assembly(ctx)?;
        for bxe in &self.bit_xor_exprs {
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&bxe.to_assembly(ctx)?);
            res.push(OpCode::Pop(Location::r10()));
            res.push(OpCode::Or(Location::r10(), Location::rax()));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.bit_xor_expr.check(ctx)?;
        for bxe in &self.bit_xor_exprs {
            bxe.check(ctx)?;
        }
        Ok(())
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
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&bae.to_assembly(ctx)?);
            res.push(OpCode::Pop(Location::r10()));
            res.push(OpCode::Xor(Location::r10(), Location::rax()));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.bit_and_expr.check(ctx)?;
        for bae in &self.bit_and_exprs {
            bae.check(ctx)?;
        }
        Ok(())
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
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&ee.to_assembly(ctx)?);
            res.push(OpCode::Pop(Location::r10()));
            res.push(OpCode::And(Location::r10(), Location::rax()));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.eq_expr.check(ctx)?;
        for ee in &self.eq_exprs {
            ee.check(ctx)?;
        }
        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.eq_expr.evaluate()?;
        for ee in &self.eq_exprs {
            res &= ee.evaluate()?;
        }
        Ok(res)
    }
}

gen_into!(EqOp -> SetCondition: [
    IsEqual => Equal,
    NotEqual => NotEqual,
]);

impl ToAssembly for EqExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.rel_expr.to_assembly(ctx)?;
        for (ro, re) in &self.rel_exprs {
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&re.to_assembly(ctx)?);
            res.push(OpCode::Pop(Location::r10()));
            res.push(OpCode::Compare(Location::rax(), Location::r10()));
            res.push(OpCode::MovImmediate(0, Location::rax()));
            res.push(OpCode::Set(ro.clone().into(), Location::al()));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.rel_expr.check(ctx)?;
        for (_, re) in &self.rel_exprs {
            re.check(ctx)?;
        }
        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.rel_expr.evaluate()?;
        for (eo, re) in &self.rel_exprs {
            res = match eo {
                EqOp::IsEqual => i32::from(re.evaluate()? == res),
                EqOp::NotEqual => i32::from(re.evaluate()? != res),
            };
        }
        Ok(res)
    }
}

gen_into!(RelOp -> SetCondition: [
    Lt => LessThan,
    Lte => LessThanOrEqual,
    Gt => GreaterThan,
    Gte => GreaterThanOrEqual,
]);

impl ToAssembly for RelExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.shift_expr.to_assembly(ctx)?;
        for (ro, se) in &self.shift_exprs {
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&se.to_assembly(ctx)?);
            res.push(OpCode::Pop(Location::r10()));
            res.push(OpCode::Compare(Location::rax(), Location::r10()));
            res.push(OpCode::MovImmediate(0, Location::rax()));
            res.push(OpCode::Set(ro.clone().into(), Location::al()));
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.shift_expr.check(ctx)?;
        for (_, se) in &self.shift_exprs {
            se.check(ctx)?;
        }
        Ok(())
    }

    fn evaluate(&self) -> Result<i32, String> {
        let mut res = self.shift_expr.evaluate()?;
        for (ro, se) in &self.shift_exprs {
            res = match ro {
                RelOp::Lt => i32::from(res < se.evaluate()?),
                RelOp::Lte => i32::from(res <= se.evaluate()?),
                RelOp::Gt => i32::from(res > se.evaluate()?),
                RelOp::Gte => i32::from(res >= se.evaluate()?),
            };
        }
        Ok(res)
    }
}

impl ToAssembly for ShiftExpr {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        let mut res = self.add_expr.to_assembly(ctx)?;
        for (so, ae) in &self.add_exprs {
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&ae.to_assembly(ctx)?);
            res.push(OpCode::Mov(Location::rax(), Location::r10()));
            res.push(OpCode::Pop(Location::rax()));
            match so {
                ShiftOp::Left => {
                    res.push(OpCode::ShiftLeft(Location::r10l(), Location::rax()));
                }
                ShiftOp::Right => {
                    res.push(OpCode::ShiftRight(Location::r10l(), Location::rax()));
                }
            }
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.add_expr.check(ctx)?;
        for (_, ae) in &self.add_exprs {
            ae.check(ctx)?;
        }
        Ok(())
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
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&term.1.to_assembly(ctx)?);
            match term.0 {
                BinaryOp::Addition => {
                    res.push(OpCode::Pop(Location::r10()));
                    res.push(OpCode::Add(Location::r10(), Location::rax()));
                }
                BinaryOp::Subtraction => {
                    res.push(OpCode::Mov(Location::rax(), Location::r10()));
                    res.push(OpCode::Pop(Location::rax()));
                    res.push(OpCode::Sub(Location::r10(), Location::rax()));
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.term.check(ctx)?;
        for term in &self.terms {
            term.1.check(ctx)?;
        }
        Ok(())
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
            res.push(OpCode::Push(Location::rax()));
            res.push_asm(&factor.1.to_assembly(ctx)?);
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push(OpCode::Pop(Location::r10()));
                    res.push(OpCode::Mult(Location::r10(), Location::rax()));
                }
                BinaryOp::Divide => {
                    res.push(OpCode::Mov(Location::rax(), Location::r10()));
                    res.push(OpCode::SignExtend);
                    res.push(OpCode::Pop(Location::rax()));
                    res.push(OpCode::IDiv(Location::r10()));
                }
                BinaryOp::Modulo => {
                    res.push(OpCode::Mov(Location::rax(), Location::r10()));
                    res.push(OpCode::SignExtend);
                    res.push(OpCode::Pop(Location::rax()));
                    res.push(OpCode::IDiv(Location::r10()));
                    res.push(OpCode::Mov(Location::rdx(), Location::rax()));
                }
                _ => unreachable!(),
            }
        }
        Ok(res)
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        self.factor.check(ctx)?;
        for factor in &self.factors {
            factor.1.check(ctx)?;
        }
        Ok(())
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
            Factor::Expr(expr) => expr.borrow().to_assembly(ctx),
            Factor::UnaryOp(uo, factor) => {
                let mut res = factor.borrow().to_assembly(ctx)?;
                match uo {
                    UnaryOp::Negation => res.push(OpCode::Negation(Location::rax())),
                    UnaryOp::Complement => res.push(OpCode::Not(Location::rax())),
                    UnaryOp::Not => {
                        res.push(OpCode::CompareImmediate(0, Location::rax()));
                        res.push(OpCode::MovImmediate(0, Location::rax()));
                        res.push(OpCode::Set(SetCondition::Equal, Location::al()));
                    }
                }
                Ok(res)
            }
            Factor::Constant(i) => {
                let mut res = Assembly::new();
                res.push(OpCode::MovImmediate(*i as i32, Location::rax()));
                Ok(res)
            }
            Factor::Identifier(postfix_id) => postfix_id.to_assembly(ctx),
            Factor::Prefix(inc_dec, id) => {
                if let Some(loc) = ctx.var_map.get(id) {
                    let mut res = Assembly::new();
                    let loc = Location {
                        name: loc.clone(),
                        size: LocationSize::Qword,
                    };
                    res.push(match inc_dec {
                        IncDec::Incremenet => OpCode::Increment(loc.clone()),
                        IncDec::Decrement => OpCode::Decrement(loc.clone()),
                    });
                    res.push(OpCode::Mov(loc, Location::rax()));
                    Ok(res)
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Factor::FunctionCall(id, args) => {
                if let Some(func) = ctx.func_map.get(id) {
                    if func.0 != args.len() {
                        Err(format!(
                            "[Code Generation]: Incorrect number of arguments for '{}'",
                            id
                        ))
                    } else {
                        let mut res = Assembly::new();

                        let locations = vec![
                            Location::rcx(),
                            Location::rdx(),
                            Location::r8(),
                            Location::r9(),
                        ];
                        for (arg, loc) in args.iter().take(locations.len()).zip(&locations) {
                            res.push_asm(&arg.to_assembly(ctx)?);
                            res.push(OpCode::Mov(Location::rax(), loc.clone()));
                        }
                        if args.len() > locations.len() {
                            for arg in args.iter().skip(locations.len()).rev() {
                                res.push_asm(&arg.to_assembly(ctx)?);
                                res.push(OpCode::Push(Location::rax()));
                            }
                        }

                        // Shadow space
                        res.push(OpCode::SubImmediate(0x28, Location::rsp()));

                        res.push(OpCode::Call(id.clone()));

                        // Remove shadow space
                        res.push(OpCode::AddImmediate(0x28, Location::rsp()));
                        if args.len() > locations.len() {
                            res.push(OpCode::AddImmediate(
                                8 * (args.len() - locations.len()) as i32,
                                Location::rsp(),
                            ));
                        }

                        Ok(res)
                    }
                } else {
                    Err(format!("[Code Generation]: Unknown function '{}'", id))
                }
            }
        }
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        match self {
            Factor::Expr(expr) => expr.borrow_mut().check(ctx),
            Factor::UnaryOp(_, factor) => factor.borrow_mut().check(ctx),
            Factor::Constant(_) => Ok(()),
            Factor::Identifier(postfix_id) => postfix_id.check(ctx),
            Factor::Prefix(_, id) => {
                if ctx.var_map.contains(id) {
                    Ok(())
                } else {
                    Err(format!("[Code Generation]: Undeclared variable '{}'", id))
                }
            }
            Factor::FunctionCall(id, args) => {
                if let Some(func) = ctx.func_map.get(id) {
                    if func.0 != args.len() {
                        Err(format!(
                            "[Code Generation]: Incorrect number of arguments for '{}'",
                            id
                        ))
                    } else {
                        for arg in args.iter().rev() {
                            arg.check(ctx)?;
                        }

                        Ok(())
                    }
                } else {
                    Err(format!("[Code Generation]: Unknown function '{}'", id))
                }
            }
        }
    }

    fn evaluate(&self) -> Result<i32, String> {
        match self {
            Factor::Expr(expr) => expr.borrow().evaluate(),
            Factor::UnaryOp(uo, factor) => match uo {
                UnaryOp::Negation => Ok(-factor.borrow().evaluate()?),
                UnaryOp::Complement => Ok(!factor.borrow().evaluate()?),
                UnaryOp::Not => Ok(i32::from(factor.borrow().evaluate()? == 0)),
            },
            Factor::Constant(i) => Ok(*i as i32),
            Factor::Identifier(postfix_id) => postfix_id.evaluate(),
            Factor::Prefix(_, _) => {
                Err("[Code Generation]: Cannot evaluate identifiers".to_string())
            }
            Factor::FunctionCall(_, _) => {
                Err("[Code Generation]: Cannot evaluate a function call".to_string())
            }
        }
    }
}

impl ToAssembly for PostfixID {
    fn to_assembly(&self, ctx: &mut Context) -> Result<Assembly, String> {
        if let Some(loc) = ctx.var_map.get(&self.id) {
            if let Some(inc_dec) = &self.postfix {
                let mut res = Assembly::new();
                let loc = Location {
                    name: loc.clone(),
                    size: LocationSize::Qword,
                };
                res.push(OpCode::Mov(loc.clone(), Location::rax()));
                res.push(match inc_dec {
                    IncDec::Incremenet => OpCode::Increment(loc),
                    IncDec::Decrement => OpCode::Decrement(loc),
                });
                Ok(res)
            } else {
                let mut res = Assembly::new();
                res.push(OpCode::Mov(
                    Location {
                        name: loc.clone(),
                        size: LocationSize::Qword,
                    },
                    Location::rax(),
                ));
                Ok(res)
            }
        } else {
            Err(format!(
                "[Code Generation]: Undeclared variable '{}'",
                self.id
            ))
        }
    }

    fn check(&self, ctx: &mut CheckContext) -> Result<(), String> {
        if ctx.var_map.contains(&self.id) {
            Ok(())
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

use std::sync::atomic::{AtomicUsize, Ordering};

use super::ast::*;

fn get_label() -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("_LABEL_{}", COUNTER.fetch_add(1, Ordering::Relaxed))
}

trait ToAssembly {
    fn to_assembly(&self) -> String;
}

impl ToAssembly for Program {
    fn to_assembly(&self) -> String {
        let mut res = String::from("    .globl main\n");
        res.push_str(self.func.to_assembly().as_str());
        res
    }
}

impl ToAssembly for Function {
    fn to_assembly(&self) -> String {
        let mut res = format!("{}:\n", self.name);
        res.push_str(self.body.to_assembly().as_str());
        res.push_str("    ret\n");
        res
    }
}

impl ToAssembly for Statement {
    fn to_assembly(&self) -> String {
        self.expr.to_assembly()
    }
}

impl ToAssembly for Expr {
    fn to_assembly(&self) -> String {
        let mut res = self.log_and_expr.to_assembly();
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
            res.push_str(lae.to_assembly().as_str());
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                    end_label
                )
                .as_str(),
            );
        }
        res
    }
}

impl ToAssembly for LogAndExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.bit_or_expr.to_assembly();
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
            res.push_str(boe.to_assembly().as_str());
            res.push_str(
                format!(
                    "    cmp $0, %rax\n    mov $0, %rax\n    setne %al\n{}:\n",
                    end_label
                )
                .as_str(),
            );
        }
        res
    }
}

impl ToAssembly for BitOrExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.bit_xor_expr.to_assembly();
        for bxe in &self.bit_xor_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bxe.to_assembly().as_str());
            res.push_str("    pop %rcx\n    or %rcx, %rax\n");
        }
        res
    }
}

impl ToAssembly for BitXorExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.bit_and_expr.to_assembly();
        for bae in &self.bit_and_exprs {
            res.push_str("    push %rax\n");
            res.push_str(bae.to_assembly().as_str());
            res.push_str("    pop %rcx\n    xor %rcx, %rax\n");
        }
        res
    }
}

impl ToAssembly for BitAndExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.eq_expr.to_assembly();
        for ee in &self.eq_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ee.to_assembly().as_str());
            res.push_str("    pop %rcx\n    and %rcx, %rax\n");
        }
        res
    }
}

impl ToAssembly for EqExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.rel_expr.to_assembly();
        for (ro, re) in &self.rel_exprs {
            res.push_str("    push %rax\n");
            res.push_str(re.to_assembly().as_str());
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

        res
    }
}

impl ToAssembly for RelExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.shift_expr.to_assembly();
        for (ro, se) in &self.shift_exprs {
            res.push_str("    push %rax\n");
            res.push_str(se.to_assembly().as_str());
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

        res
    }
}

impl ToAssembly for ShiftExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.add_expr.to_assembly();
        for (so, ae) in &self.add_exprs {
            res.push_str("    push %rax\n");
            res.push_str(ae.to_assembly().as_str());
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

        res
    }
}

impl ToAssembly for AddExpr {
    fn to_assembly(&self) -> String {
        let mut res = self.term.to_assembly();
        for term in &self.terms {
            res.push_str("    push %rax\n");
            match term.0 {
                BinaryOp::Addition => {
                    res.push_str(term.1.to_assembly().as_str());
                    res.push_str("    pop %rcx\n    add %rcx, %rax\n");
                }
                BinaryOp::Subtraction => {
                    res.push_str(term.1.to_assembly().as_str());
                    res.push_str("    mov %rax, %rcx\n    pop %rax\n    sub %rcx, %rax\n");
                }
                _ => unreachable!(),
            }
        }

        res
    }
}

impl ToAssembly for Term {
    fn to_assembly(&self) -> String {
        let mut res = self.factor.to_assembly();
        for factor in &self.factors {
            res.push_str("    push %rax\n");
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push_str(factor.1.to_assembly().as_str());
                    res.push_str("    pop %rcx\n    imul %rcx, %rax\n");
                }
                BinaryOp::Divide => {
                    res.push_str(factor.1.to_assembly().as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n");
                }
                BinaryOp::Modulo => {
                    res.push_str(factor.1.to_assembly().as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n    mov %rdx, %rax\n");
                }
                _ => unreachable!(),
            }
        }

        res
    }
}

impl ToAssembly for Factor {
    fn to_assembly(&self) -> String {
        match self {
            Factor::Expr(expr) => expr.to_assembly(),
            Factor::UnaryOp(uo, factor) => {
                let mut res = factor.to_assembly();
                match uo {
                    UnaryOp::Negation => res.push_str("    neg %rax\n"),
                    UnaryOp::Complement => res.push_str("    not %rax\n"),
                    UnaryOp::Not => {
                        res.push_str("    cmp $0, %rax\n    mov $0, %rax\n    sete %al\n")
                    }
                }
                res
            }
            Factor::Constant(i) => format!("    mov ${}, %rax\n", i),
        }
    }
}

pub fn generate(tree: &Program) -> String {
    tree.to_assembly()
}

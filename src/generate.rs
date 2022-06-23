use super::ast::*;

fn produce_assembly(expr: &Expr) -> String {
    expr.to_assembly()
}

trait ToAssembly {
    fn to_assembly(&self) -> String;
}

impl ToAssembly for Expr {
    fn to_assembly(&self) -> String {
        let mut res = self.term.to_assembly();
        for term in &self.terms {
            match term.0 {
                BinaryOp::Addition => {
                    res.push_str("    push %rax\n");
                    res.push_str(term.1.to_assembly().as_str());
                    res.push_str("    pop %rcx\n    add %rcx, %rax\n");
                }
                BinaryOp::Subtraction => {
                    res.push_str("    push %rax\n");
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
            match factor.0 {
                BinaryOp::Multiply => {
                    res.push_str("    push %rax\n");
                    res.push_str(factor.1.to_assembly().as_str());
                    res.push_str("    pop %rcx\n    imul %rcx, %rax\n");
                }
                BinaryOp::Divide => {
                    res.push_str("    push %rax\n");
                    res.push_str(factor.1.to_assembly().as_str());
                    res.push_str("    mov %rax, %rcx\n    cqo\n    pop %rax\n    idiv %rcx\n");
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
    let mut out = String::from(
        "    .globl main
main:
",
    );

    out.push_str(produce_assembly(&tree.func.body.expr).as_str());
    out.push_str("    ret\n");

    out
}

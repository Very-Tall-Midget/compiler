use super::ast::*;

fn calculate_expr(out: &mut String, expr: &Expr) {
    match expr {
        Expr::Constant(i) => {
            out.push_str(format!("{}, %eax\n", i).as_str());
        }
        Expr::UnaryOp(uo, expr) => {
            calculate_expr(out, expr);
            match uo {
                UnaryOp::Negation => {
                    out.push_str("    neg %eax\n");
                }
                UnaryOp::Complement => {
                    out.push_str("    not %eax\n");
                }
                UnaryOp::Not => {
                    out.push_str("    cmpl $0, %eax\n    movl $0, %eax\n    sete %al\n");
                }
            }
        }
    }
}

pub fn generate(tree: &Program) -> String {
    let mut out = String::from(
        "    .globl main
main:
    movl $",
    );

    calculate_expr(&mut out, &tree.func.body.expr);
    out.push_str("    ret\n");
    
    out
}

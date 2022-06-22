use super::ast::*;

pub fn generate(tree: &Program) -> String {
    return format!(
        "    .globl main
main:
    movl ${}, %eax
    ret
",
        tree.func.body.expr
    );
}

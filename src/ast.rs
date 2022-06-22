use std::{slice::Iter, rc::Rc};

use super::lexer::*;

pub fn ast(tokens: &Vec<Token>) -> Result<Program, String> {
    return tokens.iter().to_program();
}

// PROGRAM
#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

trait IntoProgram {
    fn to_program(&mut self) -> Result<Program, String>;
}

impl IntoProgram for Iter<'_, Token> {
    fn to_program(&mut self) -> Result<Program, String> {
        let func = self.to_function()?;

        if let Some(Token::EOF) = self.next() {
            Ok(Program { func })
        } else {
            Err("Expected end of file".to_string())
        }
    }
}

// FUNCTION
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

trait IntoFunction {
    fn to_function(&mut self) -> Result<Function, String>;
}

impl IntoFunction for Iter<'_, Token> {
    fn to_function(&mut self) -> Result<Function, String> {
        if let Some(Token::Keyword(Keyword::Int)) = self.next() {
            if let Some(Token::Identifier(i)) = self.next() {
                let name = i.clone();
                if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                    if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                        if let Some(Token::Symbol(Symbol::OpenBrace)) = self.next() {
                            let func = Function {
                                name,
                                body: self.to_statement()?,
                            };

                            if let Some(Token::Symbol(Symbol::CloseBrace)) = self.next() {
                                Ok(func)
                            } else {
                                Err("Expected '}'".to_string())
                            }
                        } else {
                            Err("Expected '{'".to_string())
                        }
                    } else {
                        Err("Expected ')'".to_string())
                    }
                } else {
                    Err("Expected '('".to_string())
                }
            } else {
                Err("Expected identifier".to_string())
            }
        } else {
            Err("Expected keyword 'int'".to_string())
        }
    }
}

// STATEMENT
#[derive(Debug)]
pub struct Statement {
    pub expr: Expr,
}

trait IntoStatement {
    fn to_statement(&mut self) -> Result<Statement, String>;
}

impl IntoStatement for Iter<'_, Token> {
    fn to_statement(&mut self) -> Result<Statement, String> {
        if let Some(Token::Keyword(Keyword::Return)) = self.next() {
            let expr = self.to_expr()?;
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement { expr })
            } else {
                Err("Expected ';'".to_string())
            }
        } else {
            Err("Expected keyword 'return'".to_string())
        }
    }
}

// EXPRESSION
#[derive(Debug)]
pub enum Expr {
    Constant(u32),
    UnaryOp(UnaryOp, Rc<Expr>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Negation,
    Complement,
    Not,
}

trait IntoExpr {
    fn to_expr(&mut self) -> Result<Expr, String>;
}

impl IntoExpr for Iter<'_, Token> {
    fn to_expr(&mut self) -> Result<Expr, String> {
        match self.next() {
            Some(Token::Symbol(s)) => Ok(Expr::UnaryOp(
                match s {
                    Symbol::Not => UnaryOp::Not,
                    Symbol::Complement => UnaryOp::Complement,
                    Symbol::Negation => UnaryOp::Negation,
                    _ => return Err("Expected unary operator ('!', '~' or '-') or integer".to_string()),
                },
                Rc::new(self.to_expr()?),
            )),
            Some(&Token::Integer(i)) => Ok(Expr::Constant(i)),
            _ => Err("Expected unary operator ('!', '~' or '-') or integer".to_string()),
        }
    }
}

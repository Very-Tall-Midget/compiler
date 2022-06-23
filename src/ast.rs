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

#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiply,
    Divide,
}

// EXPRESSION
#[derive(Debug)]
pub struct Expr {
    pub term: Term,
    pub terms: Vec<(BinaryOp, Term)>,
}

trait IntoExpr {
    fn to_expr(&mut self) -> Result<Expr, String>;
}

impl IntoExpr for Iter<'_, Token> {
    fn to_expr(&mut self) -> Result<Expr, String> {
        let mut terms = Vec::new();
        let term = self.to_term()?;
        let mut copy = self.clone();

        loop {
            if let Some(Token::Symbol(s)) = copy.next() {
                match s {
                    Symbol::Add => {
                        self.next();
                        terms.push((BinaryOp::Addition, self.to_term()?));
                        copy = self.clone();
                    }
                    Symbol::Negation => {
                        self.next();
                        terms.push((BinaryOp::Subtraction, self.to_term()?));
                        copy = self.clone();
                    }
                    _ => break
                }
            } else {
                break;
            }
        }

        Ok(Expr {
            term,
            terms,
        })
    }
}

// TERM
#[derive(Debug)]
pub struct Term {
    pub factor: Factor,
    pub factors: Vec<(BinaryOp, Factor)>,
}

trait IntoTerm {
    fn to_term(&mut self) -> Result<Term, String>;
}

impl IntoTerm for Iter<'_, Token> {
    fn to_term(&mut self) -> Result<Term, String> {
        let mut factors = Vec::new();
        let factor = self.to_factor()?;
        let mut copy = self.clone();

        loop {
            if let Some(Token::Symbol(s)) = copy.next() {
                match s {
                    Symbol::Mult => {
                        self.next();
                        factors.push((BinaryOp::Multiply, self.to_factor()?));
                        copy = self.clone();
                    }
                    Symbol::Div => {
                        self.next();
                        factors.push((BinaryOp::Divide, self.to_factor()?));
                        copy = self.clone();
                    }
                    _ => break
                }
            } else {
                break;
            }
        }

        Ok(Term {
            factor,
            factors,
        })
    }
}

// FACTOR
#[derive(Debug)]
pub enum Factor {
    Expr(Rc<Expr>),
    UnaryOp(UnaryOp, Rc<Factor>),
    Constant(u32),
}

trait IntoFactor {
    fn to_factor(&mut self) -> Result<Factor, String>;
}

impl IntoFactor for Iter<'_, Token> {
    fn to_factor(&mut self) -> Result<Factor, String> {
        match self.next() {
            Some(Token::Symbol(Symbol::OpenParen)) => {
                let expr = self.to_expr()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    Ok(Factor::Expr(Rc::new(expr)))
                } else {
                    Err("Expected ')'".to_string())
                }
            }
            Some(Token::Symbol(Symbol::Not)) => {
                Ok(Factor::UnaryOp(UnaryOp::Not, Rc::new(self.to_factor()?)))
            }
            Some(Token::Symbol(Symbol::Complement)) => {
                Ok(Factor::UnaryOp(UnaryOp::Complement, Rc::new(self.to_factor()?)))
            }
            Some(Token::Symbol(Symbol::Negation)) => {
                Ok(Factor::UnaryOp(UnaryOp::Negation, Rc::new(self.to_factor()?)))
            }
            Some(&Token::Integer(i)) => {
                Ok(Factor::Constant(i))
            }
            _ => Err("Expected expr, unary operator or integer".to_string())
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Negation,
    Complement,
    Not,
}

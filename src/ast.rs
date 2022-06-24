use std::{iter::Peekable, rc::Rc, slice::Iter};

use super::lexer::*;

pub fn ast(tokens: &Vec<Token>) -> Result<Program, String> {
    tokens.iter().peekable().to_tree()
}

trait ToTree<T> {
    fn to_tree(&mut self) -> Result<T, String>;
}

// PROGRAM
#[derive(Debug)]
pub struct Program {
    pub func: Function,
}

impl ToTree<Program> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Program, String> {
        let func = self.to_tree()?;

        if let Some(Token::EOF) = self.next() {
            Ok(Program { func })
        } else {
            Err("[Parser]: Expected end of file".to_string())
        }
    }
}

// FUNCTION
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Statement>,
}

impl ToTree<Function> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Function, String> {
        if let Some(Token::Keyword(Keyword::Int)) = self.next() {
            if let Some(Token::Identifier(i)) = self.next() {
                let name = i.clone();
                if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                    if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                        if let Some(Token::Symbol(Symbol::OpenBrace)) = self.next() {
                            let mut body: Vec<Statement> = Vec::new();

                            loop {
                                if let Some(_) =
                                    self.next_if_eq(&&Token::Symbol(Symbol::CloseBrace))
                                {
                                    break;
                                } else {
                                    body.push(self.to_tree()?);
                                }
                            }

                            Ok(Function { name, body })
                        } else {
                            Err("[Parser]: Expected '{'".to_string())
                        }
                    } else {
                        Err("[Parser]: Expected ')'".to_string())
                    }
                } else {
                    Err("[Parser]: Expected '('".to_string())
                }
            } else {
                Err("[Parser]: Expected identifier".to_string())
            }
        } else {
            Err("[Parser]: Expected keyword 'int'".to_string())
        }
    }
}

// STATEMENT
#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    Expr(Expr),
    Declaration(String, Option<Expr>),
}

impl ToTree<Statement> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Statement, String> {
        if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Return)) {
            let expr = self.to_tree()?;
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Return(expr))
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Int)) {
            if let Some(Token::Identifier(id)) = self.next() {
                if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Semicolon)) {
                    Ok(Statement::Declaration(id.clone(), None))
                } else if let Some(Token::Symbol(Symbol::Assignment)) = self.next() {
                    let expr = self.to_tree()?;
                    if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                        Ok(Statement::Declaration(id.clone(), Some(expr)))
                    } else {
                        Err("[Parser]: Expected ';'".to_string())
                    }
                } else {
                    Err(format!("[Parser]: Expected '=', found '{:?}'", self.next()))
                }
            } else {
                Err("[Parser]: Expected identifier".to_string())
            }
        } else {
            let expr = self.to_tree()?;
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Expr(expr))
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        }
    }
}

// EXPRESSION
#[derive(Debug)]
pub enum Expr {
    Assignment(String, Rc<Expr>),
    LogOrExpr(LogOrExpr),
}

impl ToTree<Expr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Expr, String> {
        let mut copy = self.clone();
        if let Some(Token::Identifier(id)) = copy.next_if(|&t| match t {
            Token::Identifier(_) => true,
            _ => false,
        }) {
            if let Some(Token::Symbol(Symbol::Assignment)) = copy.next() {
                self.next();
                self.next();
                Ok(Expr::Assignment(id.clone(), Rc::new(self.to_tree()?)))
            } else {
                Ok(Expr::LogOrExpr(self.to_tree()?))
            }
        } else {
            Ok(Expr::LogOrExpr(self.to_tree()?))
        }
    }
}

// LOGICAL OR EXPR
#[derive(Debug)]
pub struct LogOrExpr {
    pub log_and_expr: LogAndExpr,
    pub log_and_exprs: Vec<LogAndExpr>,
}

impl ToTree<LogOrExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<LogOrExpr, String> {
        let log_and_expr = self.to_tree()?;
        let mut log_and_exprs = Vec::new();

        while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Or)) {
            log_and_exprs.push(self.to_tree()?);
        }

        Ok(LogOrExpr {
            log_and_expr,
            log_and_exprs,
        })
    }
}

// LOGICAL AND EXPRESSION
#[derive(Debug)]
pub struct LogAndExpr {
    pub bit_or_expr: BitOrExpr,
    pub bit_or_exprs: Vec<BitOrExpr>,
}

impl ToTree<LogAndExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<LogAndExpr, String> {
        let bit_or_expr = self.to_tree()?;
        let mut bit_or_exprs = Vec::new();

        while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::And)) {
            bit_or_exprs.push(self.to_tree()?);
        }

        Ok(LogAndExpr {
            bit_or_expr,
            bit_or_exprs,
        })
    }
}

// BIT XOR EXPR
#[derive(Debug)]
pub struct BitOrExpr {
    pub bit_xor_expr: BitXorExpr,
    pub bit_xor_exprs: Vec<BitXorExpr>,
}

impl ToTree<BitOrExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<BitOrExpr, String> {
        let bit_xor_expr = self.to_tree()?;
        let mut bit_xor_exprs = Vec::new();

        while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::BitOr)) {
            bit_xor_exprs.push(self.to_tree()?);
        }

        Ok(BitOrExpr {
            bit_xor_expr,
            bit_xor_exprs,
        })
    }
}

// BIT XOR EXPR
#[derive(Debug)]
pub struct BitXorExpr {
    pub bit_and_expr: BitAndExpr,
    pub bit_and_exprs: Vec<BitAndExpr>,
}

impl ToTree<BitXorExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<BitXorExpr, String> {
        let bit_and_expr = self.to_tree()?;
        let mut bit_and_exprs = Vec::new();

        while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::BitXor)) {
            bit_and_exprs.push(self.to_tree()?);
        }

        Ok(BitXorExpr {
            bit_and_expr,
            bit_and_exprs,
        })
    }
}

// BIT AND EXPR
#[derive(Debug)]
pub struct BitAndExpr {
    pub eq_expr: EqExpr,
    pub eq_exprs: Vec<EqExpr>,
}

impl ToTree<BitAndExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<BitAndExpr, String> {
        let eq_expr = self.to_tree()?;
        let mut eq_exprs = Vec::new();

        while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::BitAnd)) {
            eq_exprs.push(self.to_tree()?);
        }

        Ok(BitAndExpr { eq_expr, eq_exprs })
    }
}

// EQUALITY EXPRESSION
#[derive(Debug)]
pub struct EqExpr {
    pub rel_expr: RelExpr,
    pub rel_exprs: Vec<(EqOp, RelExpr)>,
}

#[derive(Debug)]
pub enum EqOp {
    IsEqual,
    NotEqual,
}

impl ToTree<EqExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<EqExpr, String> {
        let rel_expr = self.to_tree()?;
        let mut rel_exprs = Vec::new();

        while let Some(t) = self.next_if(|&t| {
            t == &Token::Symbol(Symbol::IsEqual) || t == &Token::Symbol(Symbol::NotEqual)
        }) {
            rel_exprs.push((
                match t {
                    Token::Symbol(Symbol::IsEqual) => EqOp::IsEqual,
                    Token::Symbol(Symbol::NotEqual) => EqOp::NotEqual,
                    _ => unreachable!(),
                },
                self.to_tree()?,
            ));
        }

        Ok(EqExpr {
            rel_expr,
            rel_exprs,
        })
    }
}

// RELATIONAL EXPRESSION
#[derive(Debug)]
pub struct RelExpr {
    pub shift_expr: ShiftExpr,
    pub shift_exprs: Vec<(RelOp, ShiftExpr)>,
}

#[derive(Debug)]
pub enum RelOp {
    LT,
    GT,
    LTE,
    GTE,
}

impl ToTree<RelExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<RelExpr, String> {
        let shift_expr = self.to_tree()?;
        let mut shift_exprs = Vec::new();

        while let Some(t) = self.next_if(|&t| {
            t == &Token::Symbol(Symbol::LT)
                || t == &Token::Symbol(Symbol::GT)
                || t == &Token::Symbol(Symbol::LTE)
                || t == &Token::Symbol(Symbol::GTE)
        }) {
            shift_exprs.push((
                match t {
                    Token::Symbol(Symbol::LT) => RelOp::LT,
                    Token::Symbol(Symbol::GT) => RelOp::GT,
                    Token::Symbol(Symbol::LTE) => RelOp::LTE,
                    Token::Symbol(Symbol::GTE) => RelOp::GTE,
                    _ => unreachable!(),
                },
                self.to_tree()?,
            ));
        }

        Ok(RelExpr {
            shift_expr,
            shift_exprs,
        })
    }
}

// SHIFT EXPRESSION
#[derive(Debug)]
pub struct ShiftExpr {
    pub add_expr: AddExpr,
    pub add_exprs: Vec<(ShiftOp, AddExpr)>,
}

#[derive(Debug)]
pub enum ShiftOp {
    Left,
    Right,
}

impl ToTree<ShiftExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<ShiftExpr, String> {
        let add_expr = self.to_tree()?;
        let mut add_exprs = Vec::new();

        while let Some(t) = self.next_if(|&t| {
            t == &Token::Symbol(Symbol::ShiftLeft) || t == &Token::Symbol(Symbol::ShiftRight)
        }) {
            add_exprs.push((
                match t {
                    Token::Symbol(Symbol::ShiftLeft) => ShiftOp::Left,
                    Token::Symbol(Symbol::ShiftRight) => ShiftOp::Right,
                    _ => unreachable!(),
                },
                self.to_tree()?,
            ));
        }

        Ok(ShiftExpr {
            add_expr,
            add_exprs,
        })
    }
}

// ADDITIVE EXPRESSION
#[derive(Debug)]
pub struct AddExpr {
    pub term: Term,
    pub terms: Vec<(BinaryOp, Term)>,
}

#[derive(Debug)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiply,
    Divide,
    Modulo,
}

impl ToTree<AddExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<AddExpr, String> {
        let term = self.to_tree()?;
        let mut terms = Vec::new();

        while let Some(t) = self
            .next_if(|&t| t == &Token::Symbol(Symbol::Add) || t == &Token::Symbol(Symbol::Negation))
        {
            terms.push((
                match t {
                    Token::Symbol(Symbol::Add) => BinaryOp::Addition,
                    Token::Symbol(Symbol::Negation) => BinaryOp::Subtraction,
                    _ => unreachable!(),
                },
                self.to_tree()?,
            ));
        }

        Ok(AddExpr { term, terms })
    }
}

// TERM
#[derive(Debug)]
pub struct Term {
    pub factor: Factor,
    pub factors: Vec<(BinaryOp, Factor)>,
}

impl ToTree<Term> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Term, String> {
        let factor = self.to_tree()?;
        let mut factors = Vec::new();

        while let Some(t) = self.next_if(|&t| {
            t == &Token::Symbol(Symbol::Mult)
                || t == &Token::Symbol(Symbol::Div)
                || t == &Token::Symbol(Symbol::Mod)
        }) {
            factors.push((
                match t {
                    Token::Symbol(Symbol::Mult) => BinaryOp::Multiply,
                    Token::Symbol(Symbol::Div) => BinaryOp::Divide,
                    Token::Symbol(Symbol::Mod) => BinaryOp::Modulo,
                    _ => unreachable!(),
                },
                self.to_tree()?,
            ));
        }

        Ok(Term { factor, factors })
    }
}

// FACTOR
#[derive(Debug)]
pub enum Factor {
    Expr(Rc<Expr>),
    UnaryOp(UnaryOp, Rc<Factor>),
    Constant(u32),
    Identifier(String),
}

#[derive(Debug)]
pub enum UnaryOp {
    Negation,
    Complement,
    Not,
}

impl ToTree<Factor> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Factor, String> {
        match self.next() {
            Some(Token::Symbol(Symbol::OpenParen)) => {
                let expr = self.to_tree()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    Ok(Factor::Expr(Rc::new(expr)))
                } else {
                    Err("[Parser]: Expected ')'".to_string())
                }
            }
            Some(Token::Symbol(Symbol::Not)) => {
                Ok(Factor::UnaryOp(UnaryOp::Not, Rc::new(self.to_tree()?)))
            }
            Some(Token::Symbol(Symbol::Complement)) => Ok(Factor::UnaryOp(
                UnaryOp::Complement,
                Rc::new(self.to_tree()?),
            )),
            Some(Token::Symbol(Symbol::Negation)) => {
                Ok(Factor::UnaryOp(UnaryOp::Negation, Rc::new(self.to_tree()?)))
            }
            Some(&Token::Integer(i)) => Ok(Factor::Constant(i)),
            Some(Token::Identifier(id)) => Ok(Factor::Identifier(id.clone())),
            _ => Err("[Parser]: Expected expr, unary operator or integer".to_string()),
        }
    }
}

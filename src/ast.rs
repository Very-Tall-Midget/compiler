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
    pub funcs: Vec<Function>,
}

impl ToTree<Program> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Program, String> {
        let mut funcs = Vec::new();
        funcs.push(self.to_tree()?);

        loop {
            if let Some(_) = self.next_if_eq(&&Token::EOF) {
                break;
            } else {
                funcs.push(self.to_tree()?);
            }
        }
        Ok(Program { funcs })
    }
}

// FUNCTION
#[derive(Debug)]
pub struct Function {
    pub call_conv: CallingConv,
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CallingConv {
    Cdecl,
    Syscall,
}

impl ToTree<Function> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Function, String> {
        if let Some(Token::Keyword(Keyword::Int)) = self.next() {
            let mut call_conv = CallingConv::Cdecl;
            if let Some(Token::Keyword(k)) = self.next_if(|&t| match t {
                Token::Keyword(_) => true,
                _ => false,
            }) {
                match k {
                    Keyword::Cdecl => call_conv = CallingConv::Cdecl,
                    Keyword::Syscall => call_conv = CallingConv::Syscall,
                    _ => return Err("[Parser]: Expeced identifier or calling convention".to_string()),
                }
            }
            if let Some(Token::Identifier(i)) = self.next() {
                let name = i.clone();
                if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                    let mut params = Vec::new();
                    while let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Int)) {
                        if let Some(Token::Identifier(id)) = &self.next() {
                            params.push(id.clone());
                            if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Comma)) {
                                continue;
                            } else {
                                break;
                            }
                        } else {
                            return Err("[Parser]: Expected identifier".to_string());
                        }
                    }

                    if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                        if let Some(Token::Symbol(Symbol::OpenBrace)) = self.peek() {
                            if call_conv == CallingConv::Syscall {
                                Err("[Parser]: Cannot define functions with calling convention '__syscall'".to_string())
                            } else {
                                Ok(Function {
                                    call_conv,
                                    name,
                                    params,
                                    body: Some(self.to_tree()?),
                                })
                            }
                        } else if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                            Ok(Function {
                                call_conv,
                                name,
                                params,
                                body: None,
                            })
                        } else {
                            Err("[Parser]: Expected '{' or ';'".to_string())
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

// BLOCK ITEM
#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declarations),
}

impl ToTree<BlockItem> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<BlockItem, String> {
        if let Some(Token::Keyword(Keyword::Int)) = self.peek() {
            Ok(BlockItem::Declaration(self.to_tree()?))
        } else {
            Ok(BlockItem::Statement(self.to_tree()?))
        }
    }
}

// STATEMENT
#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    Expr(ExprOpt),
    If(Expr, Rc<Statement>, Option<Rc<Statement>>),
    Block(Block),
    For(ExprOpt, ExprOpt, ExprOpt, Rc<Statement>),
    ForDecl(Declarations, ExprOpt, ExprOpt, Rc<Statement>),
    While(Expr, Rc<Statement>),
    Do(Expr, Rc<Statement>),
    Break,
    Continue,
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
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::If)) {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                let expr = self.to_tree()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    let statement = Rc::new(self.to_tree()?);
                    if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Else)) {
                        Ok(Statement::If(
                            expr,
                            statement,
                            Some(Rc::new(self.to_tree()?)),
                        ))
                    } else {
                        Ok(Statement::If(expr, statement, None))
                    }
                } else {
                    Err("[Parser]: Expected ')'".to_string())
                }
            } else {
                Err("[Parser]: Expected '('".to_string())
            }
        } else if let Some(Token::Symbol(Symbol::OpenBrace)) = self.peek() {
            Ok(Statement::Block(self.to_tree()?))
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::For)) {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                if let Some(Token::Keyword(Keyword::Int)) = self.peek() {
                    let declerations = self.to_tree()?;
                    let expr1 = self.to_tree()?;
                    if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                        let expr2_cp: ExprOptCloseParen = self.to_tree()?;
                        let expr2 = expr2_cp.to_expr_opt();
                        if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                            let statement = Rc::new(self.to_tree()?);
                            Ok(Statement::ForDecl(declerations, expr1, expr2, statement))
                        } else {
                            Err("[Parser]: Expected ')'".to_string())
                        }
                    } else {
                        Err("[Parser]: Expected ';'".to_string())
                    }
                } else {
                    let expr1 = self.to_tree()?;
                    if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                        let expr2 = self.to_tree()?;
                        if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                            let expr3_cp: ExprOptCloseParen = self.to_tree()?;
                            let expr3 = expr3_cp.to_expr_opt();
                            if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                                let statement = Rc::new(self.to_tree()?);
                                Ok(Statement::For(expr1, expr2, expr3, statement))
                            } else {
                                Err("[Parser]: Expected ')'".to_string())
                            }
                        } else {
                            Err("[Parser]: Expected ';'".to_string())
                        }
                    } else {
                        Err("[Parser]: Expected ';'".to_string())
                    }
                }
            } else {
                Err("[Parser]: Expected '('".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::While)) {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                let expr = self.to_tree()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    let statement = Rc::new(self.to_tree()?);
                    Ok(Statement::While(expr, statement))
                } else {
                    Err("[Parser]: Expected ')'".to_string())
                }
            } else {
                Err("[Parser]: Expected '('".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Do)) {
            let statement = Rc::new(self.to_tree()?);
            if let Some(Token::Keyword(Keyword::While)) = self.next() {
                if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                    let expr = self.to_tree()?;
                    if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                        if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                            Ok(Statement::Do(expr, statement))
                        } else {
                            Err("[Parser]: Expected ';'".to_string())
                        }
                    } else {
                        Err("[Parser]: Expected ')'".to_string())
                    }
                } else {
                    Err("[Parser]: Expected '('".to_string())
                }
            } else {
                Err("[Parser]: Expected 'while'".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Break)) {
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Break)
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Continue)) {
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Continue)
            } else {
                Err("[Parser]: Expected ';'".to_string())
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

// BLOCK
#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

impl ToTree<Block> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Block, String> {
        let mut items = Vec::new();
        if let Some(Token::Symbol(Symbol::OpenBrace)) = self.next() {
            loop {
                if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::CloseBrace)) {
                    break Ok(Block { items });
                }
                items.push(self.to_tree()?);
            }
        } else {
            unreachable!()
        }
    }
}

// DECLATAION
#[derive(Debug)]
pub struct Declarations {
    pub declarations: Vec<(String, Option<Expr>)>,
}

impl ToTree<Declarations> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Declarations, String> {
        if let Some(_) = self.next_if_eq(&&Token::Keyword(Keyword::Int)) {
            let mut declarations = Vec::new();

            declarations.push(if let Some(Token::Identifier(id)) = self.next() {
                if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Assignment)) {
                    let expr = self.to_tree()?;
                    (id.clone(), Some(expr))
                } else {
                    (id.clone(), None)
                }
            } else {
                return Err("[Parser]: Expected identifier".to_string());
            });

            while let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Comma)) {
                declarations.push(if let Some(Token::Identifier(id)) = self.next() {
                    if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Assignment)) {
                        let expr = self.to_tree()?;
                        (id.clone(), Some(expr))
                    } else {
                        (id.clone(), None)
                    }
                } else {
                    return Err("[Parser]: Expected identifier".to_string());
                });
            }

            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Declarations { declarations })
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        } else {
            Err("[Parser]: Expected 'int'".to_string())
        }
    }
}

// EXPRESSION OPTIONS
#[derive(Debug)]
pub struct ExprOpt {
    pub expr: Option<Expr>,
}

impl ToTree<ExprOpt> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<ExprOpt, String> {
        if let Some(Token::Symbol(Symbol::Semicolon)) = self.peek() {
            Ok(ExprOpt { expr: None })
        } else {
            let expr = self.to_tree()?;
            Ok(ExprOpt { expr: Some(expr) })
        }
    }
}

// EXPRESSION OPTIONS FOR CLOSE PAREN
#[derive(Debug)]
pub struct ExprOptCloseParen {
    pub expr: Option<Expr>,
}

impl ToTree<ExprOptCloseParen> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<ExprOptCloseParen, String> {
        if let Some(Token::Symbol(Symbol::CloseParen)) = self.peek() {
            Ok(ExprOptCloseParen { expr: None })
        } else {
            let expr = self.to_tree()?;
            Ok(ExprOptCloseParen { expr: Some(expr) })
        }
    }
}

impl ExprOptCloseParen {
    fn to_expr_opt(&self) -> ExprOpt {
        ExprOpt {
            expr: self.expr.clone(),
        }
    }
}

// EXPRESSION
#[derive(Debug, Clone)]
pub enum Expr {
    Assignment(String, AssignmentOp, Rc<Expr>),
    ConditionalExpr(ConditionalExpr),
}

#[derive(Debug, Clone)]
pub enum AssignmentOp {
    Assign,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    ModAssign,
    SLAssign,
    SRAssign,
    BAndAssign,
    BXorAssign,
    BOrAssign,
}

impl ToTree<Expr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Expr, String> {
        let mut copy = self.clone();
        if let Some(Token::Identifier(id)) = copy.next_if(|&t| match t {
            Token::Identifier(_) => true,
            _ => false,
        }) {
            if let Some(Token::Symbol(s)) = copy.next_if(|&t| {
                if let Token::Symbol(s) = t {
                    match s {
                        Symbol::Assignment => true,
                        Symbol::AddAssign => true,
                        Symbol::SubAssign => true,
                        Symbol::MultAssign => true,
                        Symbol::DivAssign => true,
                        Symbol::ModAssign => true,
                        Symbol::SLAssign => true,
                        Symbol::SRAssign => true,
                        Symbol::BAndAssign => true,
                        Symbol::BXorAssign => true,
                        Symbol::BOrAssign => true,
                        _ => false,
                    }
                } else {
                    false
                }
            }) {
                self.next();
                self.next();
                Ok(Expr::Assignment(
                    id.clone(),
                    match s {
                        Symbol::Assignment => AssignmentOp::Assign,
                        Symbol::AddAssign => AssignmentOp::AddAssign,
                        Symbol::SubAssign => AssignmentOp::SubAssign,
                        Symbol::MultAssign => AssignmentOp::MultAssign,
                        Symbol::DivAssign => AssignmentOp::DivAssign,
                        Symbol::ModAssign => AssignmentOp::ModAssign,
                        Symbol::SLAssign => AssignmentOp::SLAssign,
                        Symbol::SRAssign => AssignmentOp::SRAssign,
                        Symbol::BAndAssign => AssignmentOp::BAndAssign,
                        Symbol::BXorAssign => AssignmentOp::BXorAssign,
                        Symbol::BOrAssign => AssignmentOp::BOrAssign,
                        _ => unreachable!(),
                    },
                    Rc::new(self.to_tree()?),
                ))
            } else {
                Ok(Expr::ConditionalExpr(self.to_tree()?))
            }
        } else {
            Ok(Expr::ConditionalExpr(self.to_tree()?))
        }
    }
}

// CONDITIONAL EXPR
#[derive(Debug, Clone)]
pub struct ConditionalExpr {
    pub log_or_expr: LogOrExpr,
    pub options: Option<(Rc<Expr>, Rc<Expr>)>,
}

impl ToTree<ConditionalExpr> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<ConditionalExpr, String> {
        let log_or_expr = self.to_tree()?;
        if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::QMark)) {
            let expr1 = self.to_tree()?;
            if let Some(Token::Symbol(Symbol::Colon)) = self.next() {
                let expr2 = self.to_tree()?;
                Ok(ConditionalExpr {
                    log_or_expr,
                    options: Some((Rc::new(expr1), Rc::new(expr2))),
                })
            } else {
                Err("Expected ':'".to_string())
            }
        } else {
            Ok(ConditionalExpr {
                log_or_expr,
                options: None,
            })
        }
    }
}

// LOGICAL OR EXPR
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct EqExpr {
    pub rel_expr: RelExpr,
    pub rel_exprs: Vec<(EqOp, RelExpr)>,
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct RelExpr {
    pub shift_expr: ShiftExpr,
    pub shift_exprs: Vec<(RelOp, ShiftExpr)>,
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct ShiftExpr {
    pub add_expr: AddExpr,
    pub add_exprs: Vec<(ShiftOp, AddExpr)>,
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct AddExpr {
    pub term: Term,
    pub terms: Vec<(BinaryOp, Term)>,
}

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub enum Factor {
    Expr(Rc<Expr>),
    UnaryOp(UnaryOp, Rc<Factor>),
    Constant(u32),
    Identifier(PostfixID),
    Prefix(IncDec, String),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum IncDec {
    Incremenet,
    Decrement,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negation,
    Complement,
    Not,
}

impl ToTree<Factor> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<Factor, String> {
        if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::OpenParen)) {
            let expr = self.to_tree()?;
            if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                Ok(Factor::Expr(Rc::new(expr)))
            } else {
                Err("[Parser]: Expected ')'".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Not)) {
            Ok(Factor::UnaryOp(UnaryOp::Not, Rc::new(self.to_tree()?)))
        } else if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Complement)) {
            Ok(Factor::UnaryOp(
                UnaryOp::Complement,
                Rc::new(self.to_tree()?),
            ))
        } else if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Negation)) {
            Ok(Factor::UnaryOp(UnaryOp::Negation, Rc::new(self.to_tree()?)))
        } else if let Some(&Token::Integer(i)) = self.next_if(|&t| match t {
            Token::Integer(_) => true,
            _ => false,
        }) {
            Ok(Factor::Constant(i))
        } else if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Incremenet)) {
            if let Some(Token::Identifier(id)) = self.next() {
                Ok(Factor::Prefix(IncDec::Incremenet, id.clone()))
            } else {
                Err("Expected identifier".to_string())
            }
        } else if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Decrement)) {
            if let Some(Token::Identifier(id)) = self.next() {
                Ok(Factor::Prefix(IncDec::Decrement, id.clone()))
            } else {
                Err("Expected identifier".to_string())
            }
        } else if let Some(Token::Identifier(_)) = self.peek() {
            let mut copy = self.clone();
            copy.next();
            if let Some(Token::Symbol(Symbol::OpenParen)) = copy.next() {
                if let Some(Token::Identifier(id)) = self.next() {
                    self.next();

                    if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::CloseParen)) {
                        Ok(Factor::FunctionCall(id.clone(), Vec::new()))
                    } else {
                        let mut args = Vec::new();
                        loop {
                            args.push(self.to_tree()?);
                            if let Some(_) = self.next_if_eq(&&Token::Symbol(Symbol::Comma)) {
                                continue;
                            } else if let Some(_) =
                                self.next_if_eq(&&Token::Symbol(Symbol::CloseParen))
                            {
                                break;
                            } else {
                                return Err("[Parser]: Expected ',' or ')'".to_string());
                            }
                        }
                        Ok(Factor::FunctionCall(id.clone(), args))
                    }
                } else {
                    unreachable!()
                }
            } else {
                Ok(Factor::Identifier(self.to_tree()?))
            }
        } else {
            Err("[Parser]: Expected expr, unary operator, integer or identifier".to_string())
        }
    }
}

// POSTFIX ID
#[derive(Debug, Clone)]
pub struct PostfixID {
    pub id: String,
    pub postfix: Option<IncDec>,
}

impl ToTree<PostfixID> for Peekable<Iter<'_, Token>> {
    fn to_tree(&mut self) -> Result<PostfixID, String> {
        match self.next() {
            Some(Token::Identifier(id)) => {
                if let Some(Token::Symbol(s)) = self.next_if(|&t| {
                    t == &Token::Symbol(Symbol::Incremenet)
                        || t == &Token::Symbol(Symbol::Decrement)
                }) {
                    Ok(PostfixID {
                        id: id.clone(),
                        postfix: Some(match s {
                            Symbol::Incremenet => IncDec::Incremenet,
                            Symbol::Decrement => IncDec::Decrement,
                            _ => unreachable!(),
                        }),
                    })
                } else {
                    Ok(PostfixID {
                        id: id.clone(),
                        postfix: None,
                    })
                }
            }
            _ => Err("[Parser]: Expected expr, unary operator or integer".to_string()),
        }
    }
}

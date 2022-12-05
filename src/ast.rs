use std::{cell::RefCell, iter::Peekable, rc::Rc, slice::Iter};

use super::lexer::*;

macro_rules! gen_into(
    ($from:ident -> $to:ident: [ $($match_from:pat => $match_to:expr),+ $(,)? ]) => {
        impl From<$from> for $to {
            fn from(val: $from) -> $to {
                match val {
                    $($match_from => $match_to),+,
                    _ => unreachable!(),
                }
            }
        }
    };
);

type Tree<T> = Result<T, String>;
type Tokens<'a> = Peekable<Iter<'a, Token>>;

trait ToTree<T> {
    fn to_tree(&mut self) -> Tree<T>;
}

// MAIN FUNCTION
pub fn ast(tokens: &[Token]) -> Tree<Program> {
    tokens.iter().peekable().to_tree()
}

// PROGRAM
#[derive(Debug)]
pub struct Program {
    pub items: Vec<ProgramItems>,
}

#[derive(Debug)]
pub enum ProgramItems {
    Function(Function),
    Declarations(Declarations),
}

impl ToTree<Program> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Program> {
        let mut items = Vec::new();

        loop {
            if self.next_if_eq(&&Token::Eof).is_some() {
                break;
            } else {
                let mut copy = self.clone();
                if let Some(Token::Keyword(Keyword::Int)) = copy.next() {
                    if let Some(Token::Identifier(_)) = copy.next() {
                        match copy.next() {
                            Some(Token::Symbol(Symbol::OpenParen)) => {
                                items.push(ProgramItems::Function(self.to_tree()?))
                            }
                            _ => items.push(ProgramItems::Declarations(self.to_tree()?)),
                        }
                    } else {
                        items.push(ProgramItems::Function(self.to_tree()?));
                    }
                } else {
                    return Err("[Parser]: Expected 'int'".to_string());
                }
            }
        }

        Ok(Program { items })
    }
}

// FUNCTION
#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
}

impl ToTree<Function> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Function> {
        if let Some(Token::Keyword(Keyword::Int)) = self.next() {
            if let Some(Token::Identifier(i)) = self.next() {
                let name = i.clone();
                if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                    let mut params = Vec::new();
                    while self.next_if_eq(&&Token::Keyword(Keyword::Int)).is_some() {
                        if let Some(Token::Identifier(id)) = &self.next() {
                            params.push(id.clone());
                            if self.next_if_eq(&&Token::Symbol(Symbol::Comma)).is_some() {
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
                            Ok(Function {
                                name,
                                params,
                                body: Some(self.to_tree()?),
                            })
                        } else if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                            Ok(Function {
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
    Statement(Box<Statement>),
    Declaration(Declarations),
}

impl ToTree<BlockItem> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<BlockItem> {
        if let Some(Token::Keyword(Keyword::Int)) = self.peek() {
            Ok(BlockItem::Declaration(self.to_tree()?))
        } else {
            Ok(BlockItem::Statement(Box::new(self.to_tree()?)))
        }
    }
}

// STATEMENT
#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    Expr(ExprOpt),
    If(Expr, Rc<RefCell<Statement>>, Option<Rc<RefCell<Statement>>>),
    Block(Block),
    For(
        Box<ExprOpt>,
        Box<ExprOpt>,
        Box<ExprOpt>,
        Rc<RefCell<Statement>>,
    ),
    ForDecl(
        Declarations,
        Box<ExprOpt>,
        Box<ExprOpt>,
        Rc<RefCell<Statement>>,
    ),
    While(Expr, Rc<RefCell<Statement>>),
    Do(Expr, Rc<RefCell<Statement>>),
    Break,
    Continue,
}

impl ToTree<Statement> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Statement> {
        if self.next_if_eq(&&Token::Keyword(Keyword::Return)).is_some() {
            let expr = self.to_tree()?;
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Return(expr))
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        } else if self.next_if_eq(&&Token::Keyword(Keyword::If)).is_some() {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                let expr = self.to_tree()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    let statement = Rc::new(RefCell::new(self.to_tree()?));
                    if self.next_if_eq(&&Token::Keyword(Keyword::Else)).is_some() {
                        Ok(Statement::If(
                            expr,
                            statement,
                            Some(Rc::new(RefCell::new(self.to_tree()?))),
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
        } else if self.next_if_eq(&&Token::Keyword(Keyword::For)).is_some() {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                if let Some(Token::Keyword(Keyword::Int)) = self.peek() {
                    let declerations = self.to_tree()?;
                    let expr1 = Box::new(self.to_tree()?);
                    if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                        let expr2_cp: ExprOptCloseParen = self.to_tree()?;
                        let expr2 = expr2_cp.to_expr_opt();
                        if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                            let statement = Rc::new(RefCell::new(self.to_tree()?));
                            Ok(Statement::ForDecl(
                                declerations,
                                expr1,
                                Box::new(expr2),
                                statement,
                            ))
                        } else {
                            Err("[Parser]: Expected ')'".to_string())
                        }
                    } else {
                        Err("[Parser]: Expected ';'".to_string())
                    }
                } else {
                    let expr1 = Box::new(self.to_tree()?);
                    if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                        let expr2 = Box::new(self.to_tree()?);
                        if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                            let expr3_cp: ExprOptCloseParen = self.to_tree()?;
                            let expr3 = expr3_cp.to_expr_opt();
                            if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                                let statement = Rc::new(RefCell::new(self.to_tree()?));
                                Ok(Statement::For(expr1, expr2, Box::new(expr3), statement))
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
        } else if self.next_if_eq(&&Token::Keyword(Keyword::While)).is_some() {
            if let Some(Token::Symbol(Symbol::OpenParen)) = self.next() {
                let expr = self.to_tree()?;
                if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                    let statement = Rc::new(RefCell::new(self.to_tree()?));
                    Ok(Statement::While(expr, statement))
                } else {
                    Err("[Parser]: Expected ')'".to_string())
                }
            } else {
                Err("[Parser]: Expected '('".to_string())
            }
        } else if self.next_if_eq(&&Token::Keyword(Keyword::Do)).is_some() {
            let statement = Rc::new(RefCell::new(self.to_tree()?));
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
        } else if self.next_if_eq(&&Token::Keyword(Keyword::Break)).is_some() {
            if let Some(Token::Symbol(Symbol::Semicolon)) = self.next() {
                Ok(Statement::Break)
            } else {
                Err("[Parser]: Expected ';'".to_string())
            }
        } else if self
            .next_if_eq(&&Token::Keyword(Keyword::Continue))
            .is_some()
        {
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

impl ToTree<Block> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Block> {
        let mut items = Vec::new();
        if let Some(Token::Symbol(Symbol::OpenBrace)) = self.next() {
            loop {
                if self
                    .next_if_eq(&&Token::Symbol(Symbol::CloseBrace))
                    .is_some()
                {
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

impl ToTree<Declarations> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Declarations> {
        if self.next_if_eq(&&Token::Keyword(Keyword::Int)).is_some() {
            let mut declarations = Vec::new();

            declarations.push(if let Some(Token::Identifier(id)) = self.next() {
                if self
                    .next_if_eq(&&Token::Symbol(Symbol::Assignment))
                    .is_some()
                {
                    let expr = self.to_tree()?;
                    (id.clone(), Some(expr))
                } else {
                    (id.clone(), None)
                }
            } else {
                return Err("[Parser]: Expected identifier".to_string());
            });

            while self.next_if_eq(&&Token::Symbol(Symbol::Comma)).is_some() {
                declarations.push(if let Some(Token::Identifier(id)) = self.next() {
                    if self
                        .next_if_eq(&&Token::Symbol(Symbol::Assignment))
                        .is_some()
                    {
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

impl ToTree<ExprOpt> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<ExprOpt> {
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

impl ToTree<ExprOptCloseParen> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<ExprOptCloseParen> {
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
    Assignment(String, AssignmentOp, Rc<RefCell<Expr>>),
    ConditionalExpr(Box<ConditionalExpr>),
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

gen_into!(Symbol -> AssignmentOp: [
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
]);

impl ToTree<Expr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Expr> {
        let mut copy = self.clone();
        if let Some(Token::Identifier(id)) = copy.next_if(|&t| matches!(t, Token::Identifier(_))) {
            if let Some(Token::Symbol(s)) = copy.next_if(|&t| {
                matches!(
                    t,
                    Token::Symbol(
                        Symbol::Assignment
                            | Symbol::AddAssign
                            | Symbol::SubAssign
                            | Symbol::MultAssign
                            | Symbol::DivAssign
                            | Symbol::ModAssign
                            | Symbol::SLAssign
                            | Symbol::SRAssign
                            | Symbol::BAndAssign
                            | Symbol::BXorAssign
                            | Symbol::BOrAssign
                    )
                )
            }) {
                self.next();
                self.next();
                Ok(Expr::Assignment(
                    id.clone(),
                    s.clone().into(),
                    Rc::new(RefCell::new(self.to_tree()?)),
                ))
            } else {
                Ok(Expr::ConditionalExpr(Box::new(self.to_tree()?)))
            }
        } else {
            Ok(Expr::ConditionalExpr(Box::new(self.to_tree()?)))
        }
    }
}

type ExprRef = Rc<RefCell<Expr>>;

// CONDITIONAL EXPR
#[derive(Debug, Clone)]
pub struct ConditionalExpr {
    pub log_or_expr: LogOrExpr,
    pub options: Option<(ExprRef, ExprRef)>,
}

impl ToTree<ConditionalExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<ConditionalExpr> {
        let log_or_expr = self.to_tree()?;
        if self.next_if_eq(&&Token::Symbol(Symbol::QMark)).is_some() {
            let expr1 = RefCell::new(self.to_tree()?);
            if let Some(Token::Symbol(Symbol::Colon)) = self.next() {
                let expr2 = RefCell::new(self.to_tree()?);
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

impl ToTree<LogOrExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<LogOrExpr> {
        let log_and_expr = self.to_tree()?;
        let mut log_and_exprs = Vec::new();

        while self.next_if_eq(&&Token::Symbol(Symbol::Or)).is_some() {
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

impl ToTree<LogAndExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<LogAndExpr> {
        let bit_or_expr = self.to_tree()?;
        let mut bit_or_exprs = Vec::new();

        while self.next_if_eq(&&Token::Symbol(Symbol::And)).is_some() {
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

impl ToTree<BitOrExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<BitOrExpr> {
        let bit_xor_expr = self.to_tree()?;
        let mut bit_xor_exprs = Vec::new();

        while self.next_if_eq(&&Token::Symbol(Symbol::BitOr)).is_some() {
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

impl ToTree<BitXorExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<BitXorExpr> {
        let bit_and_expr = self.to_tree()?;
        let mut bit_and_exprs = Vec::new();

        while self.next_if_eq(&&Token::Symbol(Symbol::BitXor)).is_some() {
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

impl ToTree<BitAndExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<BitAndExpr> {
        let eq_expr = self.to_tree()?;
        let mut eq_exprs = Vec::new();

        while self.next_if_eq(&&Token::Symbol(Symbol::BitAnd)).is_some() {
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

gen_into!(Token -> EqOp: [ Token::Symbol(Symbol::IsEqual) => EqOp::IsEqual, Token::Symbol(Symbol::NotEqual) => EqOp::NotEqual ]);

impl ToTree<EqExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<EqExpr> {
        let rel_expr = self.to_tree()?;
        let mut rel_exprs = Vec::new();

        while let Some(t) =
            self.next_if(|&t| matches!(t, Token::Symbol(Symbol::IsEqual | Symbol::NotEqual)))
        {
            rel_exprs.push((t.clone().into(), self.to_tree()?));
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
    Lt,
    Gt,
    Lte,
    Gte,
}

gen_into!(Token -> RelOp: [ Token::Symbol(Symbol::Lt) => RelOp::Lt, Token::Symbol(Symbol::Gt) => RelOp::Gt, Token::Symbol(Symbol::Lte) => RelOp::Lte, Token::Symbol(Symbol::Gte) => RelOp::Gte ]);

impl ToTree<RelExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<RelExpr> {
        let shift_expr = self.to_tree()?;
        let mut shift_exprs = Vec::new();

        while let Some(t) = self.next_if(|&t| {
            matches!(
                t,
                Token::Symbol(Symbol::Lt | Symbol::Gt | Symbol::Lte | Symbol::Gte)
            )
        }) {
            shift_exprs.push((t.clone().into(), self.to_tree()?));
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

gen_into!(Token -> ShiftOp: [ Token::Symbol(Symbol::ShiftLeft) => ShiftOp::Left, Token::Symbol(Symbol::ShiftRight) => ShiftOp::Right ]);

impl ToTree<ShiftExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<ShiftExpr> {
        let add_expr = self.to_tree()?;
        let mut add_exprs = Vec::new();

        while let Some(t) =
            self.next_if(|&t| matches!(t, Token::Symbol(Symbol::ShiftLeft | Symbol::ShiftRight)))
        {
            add_exprs.push((t.clone().into(), self.to_tree()?));
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

gen_into!(Token -> BinaryOp: [ Token::Symbol(Symbol::Add) => BinaryOp::Addition, Token::Symbol(Symbol::Negation) => BinaryOp::Subtraction, Token::Symbol(Symbol::Mult) => BinaryOp::Multiply, Token::Symbol(Symbol::Div) => BinaryOp::Divide, Token::Symbol(Symbol::Mod) => BinaryOp::Modulo ]);

impl ToTree<AddExpr> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<AddExpr> {
        let term = self.to_tree()?;
        let mut terms = Vec::new();

        while let Some(t) =
            self.next_if(|&t| matches!(t, Token::Symbol(Symbol::Add | Symbol::Negation)))
        {
            terms.push((t.clone().into(), self.to_tree()?));
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

impl ToTree<Term> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Term> {
        let factor = self.to_tree()?;
        let mut factors = Vec::new();

        while let Some(t) =
            self.next_if(|&t| matches!(t, Token::Symbol(Symbol::Mult | Symbol::Div | Symbol::Mod)))
        {
            factors.push((t.clone().into(), self.to_tree()?));
        }

        Ok(Term { factor, factors })
    }
}

// FACTOR
#[derive(Debug, Clone)]
pub enum Factor {
    Expr(Rc<RefCell<Expr>>),
    UnaryOp(UnaryOp, Rc<RefCell<Factor>>),
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

gen_into!(Symbol -> IncDec: [ Symbol::Increment => IncDec::Incremenet, Symbol::Decrement => IncDec::Decrement ]);

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negation,
    Complement,
    Not,
}

impl ToTree<Factor> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<Factor> {
        if self
            .next_if_eq(&&Token::Symbol(Symbol::OpenParen))
            .is_some()
        {
            let expr = RefCell::new(self.to_tree()?);
            if let Some(Token::Symbol(Symbol::CloseParen)) = self.next() {
                Ok(Factor::Expr(Rc::new(expr)))
            } else {
                Err("[Parser]: Expected ')'".to_string())
            }
        } else if self.next_if_eq(&&Token::Symbol(Symbol::Not)).is_some() {
            Ok(Factor::UnaryOp(
                UnaryOp::Not,
                Rc::new(RefCell::new(self.to_tree()?)),
            ))
        } else if self
            .next_if_eq(&&Token::Symbol(Symbol::Complement))
            .is_some()
        {
            Ok(Factor::UnaryOp(
                UnaryOp::Complement,
                Rc::new(RefCell::new(self.to_tree()?)),
            ))
        } else if self.next_if_eq(&&Token::Symbol(Symbol::Negation)).is_some() {
            Ok(Factor::UnaryOp(
                UnaryOp::Negation,
                Rc::new(RefCell::new(self.to_tree()?)),
            ))
        } else if let Some(&Token::Integer(i)) = self.next_if(|&t| matches!(t, Token::Integer(_))) {
            Ok(Factor::Constant(i))
        } else if self
            .next_if_eq(&&Token::Symbol(Symbol::Increment))
            .is_some()
        {
            if let Some(Token::Identifier(id)) = self.next() {
                Ok(Factor::Prefix(IncDec::Incremenet, id.clone()))
            } else {
                Err("Expected identifier".to_string())
            }
        } else if self
            .next_if_eq(&&Token::Symbol(Symbol::Decrement))
            .is_some()
        {
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

                    if self
                        .next_if_eq(&&Token::Symbol(Symbol::CloseParen))
                        .is_some()
                    {
                        Ok(Factor::FunctionCall(id.clone(), Vec::new()))
                    } else {
                        let mut args = Vec::new();
                        loop {
                            args.push(self.to_tree()?);
                            if self.next_if_eq(&&Token::Symbol(Symbol::Comma)).is_some() {
                                continue;
                            } else if self
                                .next_if_eq(&&Token::Symbol(Symbol::CloseParen))
                                .is_some()
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

impl ToTree<PostfixID> for Tokens<'_> {
    fn to_tree(&mut self) -> Tree<PostfixID> {
        match self.next() {
            Some(Token::Identifier(id)) => {
                if let Some(Token::Symbol(s)) = self
                    .next_if(|&t| matches!(t, Token::Symbol(Symbol::Increment | Symbol::Decrement)))
                {
                    Ok(PostfixID {
                        id: id.clone(),
                        postfix: Some(s.clone().into()),
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

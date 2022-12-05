use std::{iter::Peekable, str::Chars};

pub fn lex(code: String) -> Result<Vec<Token>, String> {
    code.chars().peekable().to_tokens()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Integer(u32),
    Symbol(Symbol),
    Eof,
}

trait ToTokens {
    fn to_tokens(&mut self) -> Result<Vec<Token>, String>;
}

impl ToTokens for Peekable<Chars<'_>> {
    fn to_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut out = Vec::new();

        while let Some(c) = self.next() {
            if c.is_whitespace() {
                continue;
            }

            if c.is_ascii_digit() {
                let mut i = c.to_digit(10).unwrap();
                while let Some(c) = self.next_if(|&c| c.is_ascii_digit()) {
                    i *= 10;
                    i += c.to_digit(10).unwrap();
                }
                out.push(Token::Integer(i));
            } else if c.is_ascii_alphanumeric() || c == '_' {
                let mut id = String::from(c);
                while let Some(c) = self.next_if(|&c| c.is_ascii_alphanumeric() || c == '_') {
                    id.push(c);
                }

                if let Some(keyword) = id.to_keyword() {
                    out.push(Token::Keyword(keyword));
                } else {
                    out.push(Token::Identifier(id));
                }
            } else {
                let s = self.to_symbol(c)?;
                if s == Symbol::Comment {
                    loop {
                        let next = self.peek();
                        if let Some('\n') = next {
                            self.next();
                            break;
                        }
                        if next.is_none() {
                            break;
                        }
                        self.next();
                    }
                } else {
                    out.push(Token::Symbol(s));
                }
            }
        }

        out.push(Token::Eof);

        Ok(out)
    }
}

// KEYWORDS
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {
    Int,
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
}

trait Keywords {
    fn to_keyword(&self) -> Option<Keyword>;
}

impl Keywords for String {
    fn to_keyword(&self) -> Option<Keyword> {
        if self == "int" {
            Some(Keyword::Int)
        } else if self == "return" {
            Some(Keyword::Return)
        } else if self == "if" {
            Some(Keyword::If)
        } else if self == "else" {
            Some(Keyword::Else)
        } else if self == "for" {
            Some(Keyword::For)
        } else if self == "while" {
            Some(Keyword::While)
        } else if self == "do" {
            Some(Keyword::Do)
        } else if self == "break" {
            Some(Keyword::Break)
        } else if self == "continue" {
            Some(Keyword::Continue)
        } else {
            None
        }
    }
}

// SYMBOLS
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    Comment, // //

    OpenBrace,  // {
    CloseBrace, // }
    OpenParen,  // (
    CloseParen, // )
    Semicolon,  // ;
    Not,        // !
    Complement, // ~
    Negation,   // -
    Add,        // +
    Mult,       // *
    Div,        // /
    Mod,        // %
    And,        // &&
    Or,         // ||
    IsEqual,    // ==
    NotEqual,   // !=
    Lt,         // <
    Gt,         // >
    Lte,        // <=
    Gte,        // >=
    ShiftLeft,  // <<
    ShiftRight, // >>
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Assignment, // =
    Comma,      // ,
    Increment,  // ++
    Decrement,  // --
    AddAssign,  // +=
    SubAssign,  // -=
    MultAssign, // *=
    DivAssign,  // /=
    ModAssign,  // %=
    SLAssign,   // <<=
    SRAssign,   // >>=
    BAndAssign, // &=
    BXorAssign, // ^=
    BOrAssign,  // |=
    Colon,      // :
    QMark,      // ?
}

trait Symbols {
    fn to_symbol(&mut self, c: char) -> Result<Symbol, String>;
}

impl Symbols for Peekable<Chars<'_>> {
    fn to_symbol(&mut self, c: char) -> Result<Symbol, String> {
        match c {
            '{' => Ok(Symbol::OpenBrace),
            '}' => Ok(Symbol::CloseBrace),
            '(' => Ok(Symbol::OpenParen),
            ')' => Ok(Symbol::CloseParen),
            ';' => Ok(Symbol::Semicolon),
            '-' => {
                if self.next_if_eq(&'-').is_some() {
                    Ok(Symbol::Decrement)
                } else if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::SubAssign)
                } else {
                    Ok(Symbol::Negation)
                }
            }
            '~' => Ok(Symbol::Complement),
            '!' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::NotEqual)
                } else {
                    Ok(Symbol::Not)
                }
            }
            '+' => {
                if self.next_if_eq(&'+').is_some() {
                    Ok(Symbol::Increment)
                } else if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::AddAssign)
                } else {
                    Ok(Symbol::Add)
                }
            }
            '*' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::MultAssign)
                } else {
                    Ok(Symbol::Mult)
                }
            }
            '/' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::DivAssign)
                } else if self.next_if_eq(&'/').is_some() {
                    Ok(Symbol::Comment)
                } else {
                    Ok(Symbol::Div)
                }
            }
            '%' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::ModAssign)
                } else {
                    Ok(Symbol::Mod)
                }
            }
            '&' => {
                if self.next_if_eq(&'&').is_some() {
                    Ok(Symbol::And)
                } else if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::BAndAssign)
                } else {
                    Ok(Symbol::BitAnd)
                }
            }
            '|' => {
                if self.next_if_eq(&'|').is_some() {
                    Ok(Symbol::Or)
                } else if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::BOrAssign)
                } else {
                    Ok(Symbol::BitOr)
                }
            }
            '=' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::IsEqual)
                } else {
                    Ok(Symbol::Assignment)
                }
            }
            '<' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::Lte)
                } else if self.next_if_eq(&'<').is_some() {
                    if self.next_if_eq(&'=').is_some() {
                        Ok(Symbol::SLAssign)
                    } else {
                        Ok(Symbol::ShiftLeft)
                    }
                } else {
                    Ok(Symbol::Lt)
                }
            }
            '>' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::Gte)
                } else if self.next_if_eq(&'>').is_some() {
                    if self.next_if_eq(&'=').is_some() {
                        Ok(Symbol::SRAssign)
                    } else {
                        Ok(Symbol::ShiftRight)
                    }
                } else {
                    Ok(Symbol::Gt)
                }
            }
            '^' => {
                if self.next_if_eq(&'=').is_some() {
                    Ok(Symbol::BXorAssign)
                } else {
                    Ok(Symbol::BitXor)
                }
            }
            ',' => Ok(Symbol::Comma),
            ':' => Ok(Symbol::Colon),
            '?' => Ok(Symbol::QMark),
            _ => Err("[Lexer]: Unexpected character".to_string()),
        }
    }
}

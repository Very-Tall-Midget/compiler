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
    EOF,
}

trait ToTokens {
    fn to_tokens(&mut self) -> Result<Vec<Token>, String>;
}

impl ToTokens for Peekable<Chars<'_>> {
    fn to_tokens(&mut self) -> Result<Vec<Token>, String> {
        let mut out = Vec::new();

        loop {
            if let Some(c) = self.next() {
                if c.is_whitespace() {
                    continue;
                }

                if c.is_ascii_digit() {
                    let mut i = c.to_digit(10).unwrap();
                    loop {
                        if let Some(c) = self.next_if(|&c| c.is_ascii_digit()) {
                            i *= 10;
                            i += c.to_digit(10).unwrap();
                        } else {
                            break;
                        }
                    }
                    out.push(Token::Integer(i));
                } else if c.is_ascii_alphanumeric() || c == '_' {
                    let mut id = String::from(c);
                    loop {
                        if let Some(c) = self.next_if(|&c| c.is_ascii_alphanumeric() || c == '_') {
                            id.push(c);
                        } else {
                            break;
                        }
                    }

                    if let Some(keyword) = id.to_keyword() {
                        out.push(Token::Keyword(keyword));
                    } else {
                        out.push(Token::Identifier(id));
                    }
                } else {
                    let s = self.to_symbol(c)?;
                    out.push(Token::Symbol(s));
                }
            } else {
                break;
            }
        }

        out.push(Token::EOF);

        Ok(out)
    }
}

// KEYWORDS
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {
    Int,
    Return,
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
        } else {
            None
        }
    }
}

// SYMBOLS
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
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
    LT,         // <
    GT,         // >
    LTE,        // <=
    GTE,        // >=
    ShiftLeft,  // <<
    ShiftRight, // >>
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Assignment, // =
    Comma,      // ,
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
            '-' => Ok(Symbol::Negation),
            '~' => Ok(Symbol::Complement),
            '!' => {
                if let Some(_) = self.next_if_eq(&'=') {
                    Ok(Symbol::NotEqual)
                } else {
                    Ok(Symbol::Not)
                }
            }
            '+' => Ok(Symbol::Add),
            '*' => Ok(Symbol::Mult),
            '/' => Ok(Symbol::Div),
            '%' => Ok(Symbol::Mod),
            '&' => {
                if let Some('&') = self.next() {
                    Ok(Symbol::And)
                } else {
                    Ok(Symbol::BitAnd)
                }
            }
            '|' => {
                if let Some('|') = self.next() {
                    Ok(Symbol::Or)
                } else {
                    Ok(Symbol::BitOr)
                }
            }
            '=' => {
                if let Some('=') = self.next() {
                    Ok(Symbol::IsEqual)
                } else {
                    Ok(Symbol::Assignment)
                }
            }
            '<' => {
                if let Some(_) = self.next_if_eq(&'=') {
                    Ok(Symbol::LTE)
                } else if let Some(_) = self.next_if_eq(&'<') {
                    Ok(Symbol::ShiftLeft)
                } else {
                    Ok(Symbol::LT)
                }
            }
            '>' => {
                if let Some(_) = self.next_if_eq(&'=') {
                    Ok(Symbol::GTE)
                } else if let Some(_) = self.next_if_eq(&'>') {
                    Ok(Symbol::ShiftRight)
                } else {
                    Ok(Symbol::GT)
                }
            }
            '^' => Ok(Symbol::BitXor),
            ',' => Ok(Symbol::Comma),
            _ => Err("[Lexer]: Unexpected character".to_string()),
        }
    }
}

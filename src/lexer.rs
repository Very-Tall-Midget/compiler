pub fn lex(code: &str) -> Result<Vec<Token>, String> {
    let mut out = Vec::new();
    let mut keyword_or_identifier = false;
    let mut buffer = String::new();
    let mut integer = false;
    let mut integer_buffer: u32 = 0;

    for c in code.chars() {
        if !(c.is_ascii_alphanumeric() || c == '_') {
            if keyword_or_identifier {
                if let Some(k) = buffer.to_keyword() {
                    out.push(Token::Keyword(k));
                } else {
                    out.push(Token::Identifier(buffer.clone()));
                }
                buffer.clear();
                keyword_or_identifier = false;
            }

            if integer {
                out.push(Token::Integer(integer_buffer));
                integer = false;
                integer_buffer = 0;
            }
        }

        if c.is_whitespace() {
            continue;
        }

        if integer {
            if !c.is_ascii_digit() {
                return Err(format!("Expected digit, found '{}'", c));
            }
            integer_buffer *= 10;
            integer_buffer += c.to_digit(10).unwrap();
            continue;
        }

        if keyword_or_identifier {
            if c.is_ascii_alphanumeric() || c == '_' {
                buffer.push(c);
            } else {
                return Err(format!("Invalid character '{}'", c));
            }
            continue;
        }

        if c.is_ascii_digit() {
            integer = true;
            integer_buffer *= 10;
            integer_buffer += c.to_digit(10).unwrap();
        } else if let Some(s) = c.to_symbol() {
            out.push(Token::Symbol(s));
        } else if c.is_ascii_alphanumeric() || c == '_' {
            keyword_or_identifier = true;
            buffer.push(c);
        } else {
            return Err(format!("Invalid character '{}'", c));
        }
    }

    if keyword_or_identifier {
        if let Some(k) = buffer.to_keyword() {
            out.push(Token::Keyword(k));
        } else {
            out.push(Token::Identifier(buffer.clone()));
        }
    }

    if integer {
        out.push(Token::Integer(integer_buffer));
    }

    out.push(Token::EOF);

    Ok(out)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Integer(u32),
    Symbol(Symbol),
    EOF,
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
}

trait Symbols {
    fn to_symbol(&self) -> Option<Symbol>;
}

impl Symbols for char {
    fn to_symbol(&self) -> Option<Symbol> {
        match &self {
            '{' => Some(Symbol::OpenBrace),
            '}' => Some(Symbol::CloseBrace),
            '(' => Some(Symbol::OpenParen),
            ')' => Some(Symbol::CloseParen),
            ';' => Some(Symbol::Semicolon),
            _ => None,
        }
    }
}

use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::str::Chars;

use super::refs::Value;

#[derive(Debug, Clone, Copy)]
pub struct Error;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}

// ---

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LParen, RParen, OpenVector,
    Dot,
    Quote,
    Identifier(&'a str),
    Const(Value)
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::OpenVector => write!(f, "#("),
            Token::Dot => write!(f, "."),
            Token::Quote => write!(f, "'"),
            Token::Identifier(cs) => write!(f, "{}", cs),
            Token::Const(v) => write!(f, "{}", v)
        }
    }
}

// ---

#[derive(Clone, Copy)]
enum Radix {
    Binary = 0b10,
    Octal = 0o10,
    Decimal = 10,
    Hexadecimal = 0x10
}

// ---

pub struct Lexer<'a> {
    chars: Chars<'a>
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Self { Self {chars: chars.chars()} }

    fn peek(&self) -> Option<char> { self.chars.as_str().chars().next() }

    fn boolean(&mut self) -> Result<Token<'a>, Error> {
        match self.chars.next() {
            Some('t') => return Ok(Token::Const(Value::TRUE)),
            Some('f') => return Ok(Token::Const(Value::FALSE)),
            _ => unreachable!()
        }
    }

    fn number(&mut self, radix: Radix) -> Result<Token<'a>, Error> {
        let mut res: Option<isize> = None;

        loop {
            match self.peek() {
                Some(c) if c.is_digit(radix as u32) => {
                    let _ = self.chars.next();
                    let m = c.to_digit(radix as u32).unwrap() as isize;
                    res = Some(match res {
                        Some(n) =>
                            n.checked_mul(radix as isize).unwrap()
                                .checked_add(m).unwrap(),
                        None => m
                    });
                },
                _ => return res.map(|n| Token::Const(Value::try_from(n).unwrap()))
                               .ok_or(Error)
            }
        }
    }

    fn identifier(&mut self) -> Result<Token<'a>, Error> {
        let cs = self.chars.as_str();
        let mut len = 0;

        loop {
            match self.peek() {
                Some(c) if c.is_alphabetic() || c == '!' => {
                    let _ = self.chars.next();
                    len += 1;
                },
                _ => return Ok(Token::Identifier(&cs[0..len]))
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        use Token::*;

        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    let _ = self.chars.next();
                },
                Some('(') => {
                    let _ = self.chars.next();
                    return Some(Ok(LParen));
                },
                Some(')') => {
                    let _ = self.chars.next();
                    return Some(Ok(RParen));
                },
                Some('.') => {
                    let _ = self.chars.next();
                    return Some(Ok(Dot));
                },
                Some('\'') => {
                    let _ = self.chars.next();
                    return Some(Ok(Quote));
                },
                Some('#') => {
                    let _ = self.chars.next();
                    match self.peek() {
                        Some('t') => return Some(self.boolean()),
                        Some('f') => return Some(self.boolean()),
                        Some('(') => {
                            let _ = self.chars.next();
                            return Some(Ok(OpenVector));
                        }
                        _ => unimplemented!()
                    }
                },
                Some(c) if c.is_digit(10) => return Some(self.number(Radix::Decimal)),
                Some(c) if c.is_alphabetic() || c == '!' => return Some(self.identifier()),
                None => return None,
                _ => unimplemented!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        use Token::*;

        let input = "  (23 #f foo)  ";
        let chars: Vec<Token> = Lexer::new(&input).map(Result::unwrap).collect();
        assert_eq!(chars, vec![LParen, Const(23i16.into()), Const(Value::FALSE), Identifier("foo"), RParen]);
    }
}


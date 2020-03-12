use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::iter::Peekable;

use super::objects::{PgsString, Symbol};
use super::refs::Value;
use super::state::State;

#[derive(Debug, Clone, Copy)]
pub enum Error {
    Eof
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}

// ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    OpenVector,
    Dot,
    Quote,
    Identifier(Symbol),
    Const(Value)
}

impl Display for Token {
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

const SPECIAL_INITIALS: &str = "!$%&*/:<=>?^_~";

fn is_initial(c: char) -> bool { c.is_alphabetic() || SPECIAL_INITIALS.contains(c) }

const SPECIAL_SUBSEQUENTS: &str = "+-.@";

fn is_subsequent(c: char) -> bool {
    is_initial(c) || c.is_digit(10) || SPECIAL_SUBSEQUENTS.contains(c)
}

// ---

pub struct Lexer<I: Iterator<Item = char>> {
    head: Option<Result<Token, Error>>,
    chars: Peekable<I>,
    buf: String
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(chars: I) -> Self {
        Self { head: None, chars: chars.peekable(), buf: String::new() }
    }

    fn peek_char(&mut self) -> Option<char> { self.chars.peek().map(|&c| c) }

    fn boolean(&mut self) -> Result<Token, Error> {
        match self.chars.next() {
            Some('t') => return Ok(Token::Const(Value::TRUE)),
            Some('f') => return Ok(Token::Const(Value::FALSE)),
            _ => unreachable!()
        }
    }

    fn number(&mut self, radix: Radix) -> Result<Token, Error> {
        let mut res: Option<isize> = None;

        loop {
            match self.peek_char() {
                Some(c) if c.is_digit(radix as u32) => {
                    let _ = self.chars.next();
                    let m = c.to_digit(radix as u32).unwrap() as isize;
                    res = Some(match res {
                        Some(n) => n.checked_mul(radix as isize).unwrap().checked_add(m).unwrap(),
                        None => m
                    });
                },
                _ => return Ok(Token::Const(Value::try_from(res.unwrap()).unwrap()))
            }
        }
    }

    unsafe fn identifier(&mut self, state: &mut State) -> Result<Token, Error> {
        self.buf.clear();
        self.buf.push(self.chars.next().unwrap()); // first char, already checked

        loop {
            match self.peek_char() {
                Some(c) if is_subsequent(c) || c == '!' => {
                    let _ = self.chars.next();
                    self.buf.push(c);
                },
                _ =>
                    return Ok(Token::Identifier(Symbol::new(state, &self.buf).unwrap_or_else(
                        || {
                            state.collect_garbage();
                            Symbol::new(state, &self.buf).unwrap()
                        }
                    ))),
            }
        }
    }

    unsafe fn string(&mut self, state: &mut State) -> Result<Token, Error> {
        let _ = self.chars.next(); // must be '"', skip it

        self.buf.clear();

        loop {
            match self.peek_char() {
                Some('"') => {
                    let _ = self.chars.next();
                    return Ok(Token::Const(
                        PgsString::new(state, &self.buf)
                            .unwrap_or_else(|| {
                                state.collect_garbage();
                                PgsString::new(state, &self.buf).unwrap()
                            })
                            .into()
                    ));
                },
                Some(c) => {
                    let _ = self.chars.next();
                    self.buf.push(c);
                },
                None => return Err(Error::Eof)
            }
        }
    }

    pub unsafe fn peek(&mut self, state: &mut State) -> Option<Result<Token, Error>> {
        if self.head.is_none() {
            self.head = self.next(state);
        }

        self.head
    }

    pub unsafe fn next(&mut self, state: &mut State) -> Option<Result<Token, Error>> {
        use Token::*;

        let head = self.head.take();
        if head.is_some() {
            head
        } else {
            loop {
                match self.peek_char() {
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
                    Some('"') => return Some(self.string(state)),
                    Some('#') => {
                        let _ = self.chars.next();
                        match self.peek_char() {
                            Some('t') => return Some(self.boolean()),
                            Some('f') => return Some(self.boolean()),
                            Some('(') => {
                                let _ = self.chars.next();
                                return Some(Ok(OpenVector));
                            },
                            _ => unimplemented!()
                        }
                    },
                    Some(c) if c.is_digit(10) => return Some(self.number(Radix::Decimal)),
                    Some(c) if is_initial(c) => return Some(self.identifier(state)),
                    None => return None,
                    _ => unimplemented!()
                }
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

        let mut state = State::new(&[], 1 << 12, 1 << 20);
        let foo: Symbol = Symbol::new(&mut state, "foo").unwrap();

        let input = "  (23 #f foo )  ";
        let mut lexer = Lexer::new(input.chars());

        let mut tokens = Vec::new();

        while let Some(tok) = unsafe { lexer.next(&mut state) } {
            tokens.push(tok.unwrap());
        }

        assert_eq!(tokens, vec![
            LParen,
            Const(23i16.into()),
            Const(Value::FALSE),
            Identifier(foo),
            RParen
        ]);
    }
}

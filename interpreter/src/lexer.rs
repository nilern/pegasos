use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::iter::Peekable;

use super::objects::{PgsString, Symbol};
use super::refs::{StatefulDisplay, Value};
use super::state::State;

#[derive(Debug, Clone, Copy)]
pub struct Error {
    pub what: ErrorWhat,
    pub at: Pos
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorWhat {
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

impl StatefulDisplay for Token {
    fn st_fmt(&self, state: &State, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::OpenVector => write!(f, "#("),
            Token::Dot => write!(f, "."),
            Token::Quote => write!(f, "'"),
            Token::Identifier(cs) => write!(f, "{}", cs),
            Token::Const(v) => write!(f, "{}", v.fmt_wrap(state))
        }
    }
}

// ---

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos {
    pub line: usize,
    pub column: usize
}

impl Default for Pos {
    fn default() -> Self { Self { line: 1, column: 1 } }
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{}:{}", self.line, self.column) }
}

// ---

struct PosChars<I: Iterator<Item = char>> {
    inner: Peekable<I>,
    pos: Pos
}

impl<I: Iterator<Item = char>> PosChars<I> {
    fn new(inner: Peekable<I>) -> Self { Self { inner, pos: Pos::default() } }

    fn peek(&mut self) -> Option<(Pos, char)> {
        let pos = self.pos;
        self.inner.peek().map(|&c| (pos, c))
    }

    fn pos(&self) -> Pos { self.pos }
}

impl<I: Iterator<Item = char>> Iterator for PosChars<I> {
    type Item = (Pos, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|c| {
            let pos = self.pos;
            self.pos = if c == '\n' {
                Pos { line: pos.line + 1, column: 1 }
            } else {
                Pos { column: pos.column + 1, ..pos }
            };
            (pos, c)
        })
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
    head: Option<Result<(Pos, Token), Error>>,
    chars: PosChars<I>,
    buf: String
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(chars: I) -> Self {
        Self { head: None, chars: PosChars::new(chars.peekable()), buf: String::new() }
    }

    pub fn pos(&self) -> Pos {
        match self.head {
            Some(Ok((pos, _))) => pos,
            Some(Err(Error { at, .. })) => at,
            None => self.chars.pos()
        }
    }

    fn peek_char(&mut self) -> Option<char> { self.chars.peek().map(|c| c.1) }

    fn boolean(&mut self, pos: Pos) -> Result<(Pos, Token), Error> {
        match self.chars.next() {
            Some((_, 't')) => return Ok((pos, Token::Const(Value::TRUE))),
            Some((_, 'f')) => return Ok((pos, Token::Const(Value::FALSE))),
            _ => unreachable!()
        }
    }

    fn number(&mut self, radix: Radix) -> Result<(Pos, Token), Error> {
        let (pos, sign) = match self.chars.peek().unwrap() {
            (pos, '+') => {
                let _ = self.chars.next().unwrap();
                (pos, 1)
            },
            (pos, '-') => {
                let _ = self.chars.next().unwrap();
                (pos, -1)
            },
            (pos, _) => (pos, 1)
        };

        let (_, c) = self.chars.next().unwrap();
        let mut res = c.to_digit(radix as u32).unwrap() as isize * sign;

        loop {
            match self.peek_char() {
                Some(c) if c.is_digit(radix as u32) => {
                    let _ = self.chars.next().unwrap();
                    let m = c.to_digit(radix as u32).unwrap() as isize;
                    res = res.checked_mul(radix as isize).unwrap().checked_add(m).unwrap();
                },
                _ => return Ok((pos, Token::Const(Value::try_from(res).unwrap())))
            }
        }
    }

    unsafe fn identifier(
        &mut self, state: &mut State, prefix: &str
    ) -> Result<(Pos, Token), Error> {
        let (pos, initial) = self.chars.next().unwrap(); // already checked that `is_initial`
        self.buf.clear();
        self.buf.push_str(prefix);
        self.buf.push(initial);

        loop {
            match self.peek_char() {
                Some(c) if is_subsequent(c) => {
                    let _ = self.chars.next();
                    self.buf.push(c);
                },
                _ =>
                    return Ok((
                        pos,
                        Token::Identifier(Symbol::new(state, &self.buf).unwrap_or_else(|| {
                            state.collect_garbage();
                            Symbol::new(state, &self.buf).unwrap()
                        }))
                    )),
            }
        }
    }

    fn character(&mut self, pos: Pos) -> Result<(Pos, Token), Error> {
        match self.chars.next() {
            Some((_, c)) => Ok((pos, Token::Const(c.into()))),
            None => Err(Error { what: ErrorWhat::Eof, at: pos })
        }
    }

    unsafe fn string(&mut self, state: &mut State) -> Result<(Pos, Token), Error> {
        let (pos, _) = self.chars.next().unwrap(); // must be '"', skip it

        self.buf.clear();

        loop {
            match self.peek_char() {
                Some('"') => {
                    let _ = self.chars.next();
                    return Ok((
                        pos,
                        Token::Const(
                            PgsString::new(state, &self.buf)
                                .unwrap_or_else(|| {
                                    state.collect_garbage();
                                    PgsString::new(state, &self.buf).unwrap()
                                })
                                .into()
                        )
                    ));
                },
                Some(c) => {
                    let _ = self.chars.next();
                    self.buf.push(c);
                },
                None => return Err(Error { what: ErrorWhat::Eof, at: pos })
            }
        }
    }

    pub unsafe fn peek(&mut self, state: &mut State) -> Option<Result<(Pos, Token), Error>> {
        if self.head.is_none() {
            self.head = self.next(state);
        }

        self.head
    }

    pub unsafe fn next(&mut self, state: &mut State) -> Option<Result<(Pos, Token), Error>> {
        use Token::*;

        self.head.take().or_else(|| loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    let _ = self.chars.next();
                },
                Some(';') => {
                    let _ = self.chars.next();

                    loop {
                        match self.peek_char() {
                            Some('\n') => break,
                            Some(_) => {
                                let _ = self.chars.next();
                            },
                            None => break
                        }
                    }
                },
                Some('(') => {
                    let (pos, _) = self.chars.next().unwrap();
                    break Some(Ok((pos, LParen)));
                },
                Some(')') => {
                    let (pos, _) = self.chars.next().unwrap();
                    break Some(Ok((pos, RParen)));
                },
                Some('.') => {
                    let (pos, _) = self.chars.next().unwrap();
                    break Some(Ok((pos, Dot)));
                },
                Some('\'') => {
                    let (pos, _) = self.chars.next().unwrap();
                    break Some(Ok((pos, Quote)));
                },
                Some('"') => break Some(self.string(state)),
                Some('#') => {
                    let (pos, _) = self.chars.next().unwrap();

                    match self.peek_char() {
                        Some('t') => break Some(self.boolean(pos)),
                        Some('f') => break Some(self.boolean(pos)),
                        Some('(') => {
                            let _ = self.chars.next().unwrap();
                            break Some(Ok((pos, OpenVector)));
                        },
                        Some('#') => {
                            let _ = self.chars.next().unwrap();
                            break Some(self.identifier(state, "##"));
                        },
                        Some('\\') => {
                            let _ = self.chars.next().unwrap();
                            break Some(self.character(pos));
                        },
                        _ => unimplemented!()
                    }
                },
                Some(c) if is_initial(c) => break Some(self.identifier(state, "")),
                Some(c) if c.is_digit(10) || c == '+' || c == '-' =>
                    break Some(self.number(Radix::Decimal)),
                Some(c) => unimplemented!("{:?}", c),
                None => break None
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        use Token::*;

        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let foo: Symbol = Symbol::new(&mut state, "foo").unwrap();

        let input = "  (23 #f\n foo )  ";
        let mut lexer = Lexer::new(input.chars());

        let mut tokens = Vec::new();

        while let Some(res) = unsafe { lexer.next(&mut state) } {
            tokens.push(res.unwrap());
        }

        assert_eq!(tokens, vec![
            (Pos { line: 1, column: 3 }, LParen),
            (Pos { line: 1, column: 4 }, Const(23i16.into())),
            (Pos { line: 1, column: 7 }, Const(Value::FALSE)),
            (Pos { line: 2, column: 2 }, Identifier(foo)),
            (Pos { line: 2, column: 6 }, RParen)
        ]);
    }
}

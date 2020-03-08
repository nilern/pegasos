use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::iter::Peekable;

use super::lexer::{self, Token, Lexer};
use super::state::State;
use super::objects::{Symbol, Pair, Vector};
use super::refs::Value;

#[derive(Debug)]
pub enum Error<'a> {
    Lex(lexer::Error),
    Expected {
        expected: lexer::Token<'a>,
        actual: lexer::Token<'a>
    },
    Eof
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Lex(lex_err) => write!(f, "Lexical error: {}.", lex_err),
            Error::Expected {expected, actual} => write!(f, "Was expecting '{}' but got '{}'.", expected, actual),
            Error::Eof => write!(f, "Incomplete datum, input ran out.")
        }
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Peekable<Lexer<'a>>) -> Self { Self {lexer} }

    pub fn sexpr(&mut self, state: &mut State) -> Option<Result<(), Error>> {
        use Token::*;

        match self.lexer.peek() {
            Some(Ok(LParen)) => {
                let _ = self.lexer.next();
                let mut len = 0;
                
                loop {
                    match self.lexer.peek() {
                        Some(Ok(RParen)) => {
                            let _ = self.lexer.next();

                            state.push(Value::NIL);
                            for _ in 0..len {
                                unsafe { state.cons(); }
                            }
                            return Some(Ok(()));
                        },
                        Some(Ok(Dot)) => {
                            let _ = self.lexer.next();
                            self.sexpr(state)?;
                            match self.lexer.next() {
                                Some(Ok(RParen)) => {},
                                Some(Ok(actual)) => return Some(Err(Error::Expected {expected: RParen, actual})),
                                Some(Err(lex_err)) => return Some(Err(Error::Lex(lex_err))),
                                None => return Some(Err(Error::Eof))
                            }

                            for _ in 0..len {
                                unsafe { state.cons(); }
                            }
                            return Some(Ok(()));
                        }
                        Some(_) => {
                            self.sexpr(state)?;
                            len += 1;
                        },
                        Some(Err(lex_err)) => return Some(Err(Error::Lex(*lex_err))),
                        None => return Some(Err(Error::Eof))
                    }
                }
            },
            Some(Ok(OpenVector)) => {
                let _ = self.lexer.next();
                let mut len = 0;

                loop {
                    match self.lexer.peek() {
                        Some(Ok(RParen)) => {
                            let _ = self.lexer.next();
                            unsafe { state.vector(len); }
                            return Some(Ok(()));
                        },
                        Some(Err(lex_err)) => return Some(Err(Error::Lex(*lex_err))),
                        Some(_) => {
                            self.sexpr(state)?;
                            len += 1;
                        },
                        None => return Some(Err(Error::Eof))
                    }
                }
            },
            Some(Ok(Quote)) => {
                let _ = self.lexer.next();
                unsafe { state.push_symbol("quote") };
                self.sexpr(state)?;
                state.push(Value::NIL);
                unsafe {
                    state.cons();
                    state.cons();
                }
                Some(Ok(()))
            },
            Some(Ok(Identifier(_))) => {
                if let Some(Ok(Identifier(cs))) = self.lexer.next() {
                    unsafe { state.push_symbol(cs); }
                    Some(Ok(()))
                } else {
                    unreachable!()
                }
            }
            Some(Ok(Const(_))) => {
                if let Some(Ok(Const(v))) = self.lexer.next() {
                    state.push(v);
                    Some(Ok(()))
                } else {
                    unreachable!()
                }
            },
            Some(Err(lex_err)) => Some(Err(Error::Lex(*lex_err))),
            Some(_) => unimplemented!(),
            None => None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ").peekable());
        parser.sexpr(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::from(23i16));
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" foo ").peekable());
        unsafe { state.push_symbol("foo"); }
        let symbol: Symbol = state.pop().unwrap().try_into().unwrap();

        parser.sexpr(&mut state).unwrap();
        let parsed: Value = state.pop().unwrap();
        let parsed: Symbol = parsed.try_into().unwrap();

        assert_eq!(parsed.as_str(), symbol.as_str());
        assert_eq!(parsed.hash, symbol.hash);
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" () ").peekable());

        parser.sexpr(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::NIL);
    }

    #[test]
    fn test_proper() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5) ").peekable());

        parser.sexpr(&mut state).unwrap();
        let parsed: Pair = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.car, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::NIL);
    }

    #[test]
    fn test_improper() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5 . 8) ").peekable());

        parser.sexpr(&mut state).unwrap();
        let parsed: Pair = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.car, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::from(8i16));
    }

    #[test]
    fn test_vector() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" #(5) ").peekable());

        parser.sexpr(&mut state).unwrap();
        let parsed: Vector = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0], Value::from(5i16));
    }
}


use std::fmt::{self, Display, Formatter};

use super::lexer::{self, Token, Lexer};
use super::state::State;
use super::objects::{Symbol, Pair, Vector};
use super::refs::Value;

#[derive(Debug)]
pub enum Error {
    Lex(lexer::Error),
    Expected {
        expected: lexer::Token,
        actual: lexer::Token
    },
    Eof
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Lex(lex_err) => write!(f, "Lexical error: {}.", lex_err),
            Error::Expected {expected, actual} => write!(f, "Was expecting '{}' but got '{}'.", expected, actual),
            Error::Eof => write!(f, "Incomplete datum, input ran out.")
        }
    }
}

pub struct Parser<I: Iterator<Item=char>> {
    lexer: Lexer<I>
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Self { Self {lexer} }

    pub unsafe fn sexpr(&mut self, state: &mut State) -> Result<Option<()>, Error> {
        use Token::*;

        match self.lexer.peek(state) {
            Some(Ok(LParen)) => {
                let _ = self.lexer.next(state);
                let mut len = 0;
                
                loop {
                    match self.lexer.peek(state) {
                        Some(Ok(RParen)) => {
                            let _ = self.lexer.next(state);

                            state.push(Value::NIL);
                            for _ in 0..len {
                                state.cons();
                            }
                            return Ok(Some(()));
                        },
                        Some(Ok(Dot)) => {
                            let _ = self.lexer.next(state);

                            self.sexpr(state).and_then(|v| v.ok_or(Error::Eof))?;
                            match self.lexer.next(state) {
                                Some(Ok(RParen)) => {},
                                Some(Ok(actual)) => return Err(Error::Expected {expected: RParen, actual}),
                                Some(Err(lex_err)) => return Err(Error::Lex(lex_err)),
                                None => return Err(Error::Eof)
                            }

                            for _ in 0..len {
                                state.cons();
                            }
                            return Ok(Some(()));
                        },
                        Some(Err(lex_err)) => return Err(Error::Lex(lex_err)),
                        Some(_) => {
                            self.sexpr(state).and_then(|v| v.ok_or(Error::Eof))?;
                            len += 1;
                        },
                        None => return Err(Error::Eof)
                    }
                }
            },
            Some(Ok(OpenVector)) => {
                let _ = self.lexer.next(state);
                let mut len = 0;

                loop {
                    match self.lexer.peek(state) {
                        Some(Ok(RParen)) => {
                            let _ = self.lexer.next(state);
                            state.vector(len);
                            return Ok(Some(()));
                        },
                        Some(Err(lex_err)) => return Err(Error::Lex(lex_err)),
                        Some(_) => {
                            self.sexpr(state).and_then(|v| v.ok_or(Error::Eof))?;
                            len += 1;
                        },
                        None => return Err(Error::Eof)
                    }
                }
            },
            Some(Ok(Quote)) => {
                let _ = self.lexer.next(state);
                state.push_symbol("quote");
                self.sexpr(state).and_then(|v| v.ok_or(Error::Eof))?;
                state.push(Value::NIL);
                state.cons();
                state.cons();
                Ok(Some(()))
            },
            Some(Ok(Identifier(_))) => {
                if let Some(Ok(Identifier(sym))) = self.lexer.next(state) {
                    state.push(sym);
                    Ok(Some(()))
                } else {
                    unreachable!()
                }
            }
            Some(Ok(Const(_))) => {
                if let Some(Ok(Const(v))) = self.lexer.next(state) {
                    state.push(v);
                    Ok(Some(()))
                } else {
                    unreachable!()
                }
            },
            Some(Err(lex_err)) => Err(Error::Lex(lex_err)),
            Some(_) => unimplemented!(),
            None => Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryInto;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ".chars()));
        unsafe { parser.sexpr(&mut state).unwrap(); }
        assert_eq!(state.pop().unwrap(), Value::from(23i16));
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" foo ".chars()));
        unsafe { state.push_symbol("foo"); }
        let symbol: Symbol = state.pop().unwrap().try_into().unwrap();

        unsafe { parser.sexpr(&mut state).unwrap(); }
        let parsed: Value = state.pop().unwrap();
        let parsed: Symbol = parsed.try_into().unwrap();

        assert_eq!(parsed.as_str(), symbol.as_str());
        assert_eq!(parsed.hash, symbol.hash);
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" () ".chars()));

        unsafe { parser.sexpr(&mut state).unwrap(); }

        assert_eq!(state.pop().unwrap(), Value::NIL);
    }

    #[test]
    fn test_proper() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5) ".chars()));

        unsafe { parser.sexpr(&mut state).unwrap(); }
        let parsed: Pair = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.car, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::NIL);
    }

    #[test]
    fn test_improper() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5 . 8) ".chars()));

        unsafe { parser.sexpr(&mut state).unwrap(); }
        let parsed: Pair = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.car, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::from(8i16));
    }

    #[test]
    fn test_vector() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" #(5) ".chars()));

        unsafe { parser.sexpr(&mut state).unwrap(); }
        let parsed: Vector = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0], Value::from(5i16));
    }
}


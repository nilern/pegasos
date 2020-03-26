use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};

use super::lexer::{self, Lexer, Pos, Token};
use super::objects::Pair;
use super::refs::{DynamicDowncast, Value};
use super::state::State;

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub source: Value,
    pub pos: Pos
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{}:{}", self.source, self.pos) }
}

#[derive(Debug)]
pub struct Error {
    what: ErrorWhat,
    at: Loc
}

impl Error {
    fn from_lex(lex_err: lexer::Error, source: Value) -> Self {
        Self { what: ErrorWhat::Lex(lex_err), at: Loc { source, pos: lex_err.at } }
    }
}

#[derive(Debug)]
pub enum ErrorWhat {
    Lex(lexer::Error),
    Expected { expected: lexer::Token, actual: lexer::Token },
    Unexpected(lexer::Token),
    Eof
}

impl Display for ErrorWhat {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorWhat::Lex(lex_err) => write!(f, "Lexical error: {}.", lex_err),
            ErrorWhat::Expected { expected, actual } =>
                write!(f, "Was expecting '{}' but got '{}'.", expected, actual),
            ErrorWhat::Unexpected(tok) => write!(f, "Unexpected '{}'.", tok),
            ErrorWhat::Eof => write!(f, "Incomplete datum, input ran out.")
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{} at {}", self.what, self.at) }
}

pub struct Parser<I: Iterator<Item = char>> {
    lexer: Lexer<I>
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Self { Self { lexer } }

    /// { source -- datum }
    unsafe fn sexpr(&mut self, state: &mut State) -> Result<Option<()>, Error> {
        use Token::*;

        match self.lexer.peek(state) {
            Some(Ok((pos, LParen))) => {
                let _ = self.lexer.next(state);
                let mut count = 0;
                let mut proper = true;

                let res = loop {
                    // { source datum{count} }
                    match self.lexer.peek(state) {
                        Some(Ok((_, RParen))) => {
                            let _ = self.lexer.next(state);
                            break Ok(Some(()));
                        },
                        Some(Ok((_, Dot))) => {
                            let _ = self.lexer.next(state);
                            state.push(state.get(count).unwrap()); // { source datum{count} source }

                            match self.sexpr(state) {
                                Ok(Some(())) => {
                                    count += 1;
                                    proper = false;
                                },
                                Ok(None) =>
                                    break Err(Error {
                                        what: ErrorWhat::Eof,
                                        at: Loc {
                                            source: state.get(count).unwrap(),
                                            pos: self.lexer.pos()
                                        }
                                    }),
                                res @ Err(_) => break res
                            } // { source datum{count} }

                            match self.lexer.next(state) {
                                Some(Ok((_, RParen))) => {},
                                Some(Ok((pos, actual))) =>
                                    break Err(Error {
                                        what: ErrorWhat::Expected { expected: RParen, actual },
                                        at: Loc { source: state.get(count).unwrap(), pos }
                                    }),
                                Some(Err(lex_err)) =>
                                    break Err(Error::from_lex(lex_err, state.get(count).unwrap())),
                                None =>
                                    break Err(Error {
                                        what: ErrorWhat::Eof,
                                        at: Loc {
                                            source: state.get(count).unwrap(),
                                            pos: self.lexer.pos()
                                        }
                                    }),
                            }

                            break Ok(Some(()));
                        },
                        Some(Err(lex_err)) =>
                            break Err(Error::from_lex(lex_err, state.get(count).unwrap())),
                        Some(_) => {
                            state.push(state.get(count).unwrap()); // { source datum{count} source }

                            match self.sexpr(state) {
                                Ok(Some(())) => count += 1,
                                Ok(None) =>
                                    break Err(Error {
                                        what: ErrorWhat::Eof,
                                        at: Loc {
                                            source: state.get(count).unwrap(),
                                            pos: self.lexer.pos()
                                        }
                                    }),
                                res @ Err(_) => break res
                            }
                        },
                        None =>
                            break Err(Error {
                                what: ErrorWhat::Eof,
                                at: Loc {
                                    source: state.get(count).unwrap(),
                                    pos: self.lexer.pos()
                                }
                            }),
                    }
                }; // { source datum{count} }

                match res {
                    Ok(_) => {
                        if proper {
                            state.push(Value::NIL);
                        } else {
                            count -= 1;
                        }
                        for _ in 0..count {
                            state.cons();
                        }
                        let loc = Loc { source: state.remove(1).unwrap(), pos }; // { datum }
                        state.push_syntax(loc).unwrap(); // { syntax }
                    },
                    Err(_) => {
                        for _ in 0..count + 1 {
                            state.pop::<Value>().unwrap().unwrap();
                        } // { }
                    }
                }

                res
            },
            Some(Ok((pos, OpenVector))) => {
                let _ = self.lexer.next(state);
                let mut len = 0;

                let res = loop {
                    match self.lexer.peek(state) {
                        Some(Ok((_, RParen))) => {
                            let _ = self.lexer.next(state);
                            break Ok(Some(()));
                        },
                        Some(Err(lex_err)) =>
                            break Err(Error::from_lex(lex_err, state.get(len).unwrap())),
                        Some(_) => {
                            state.push(state.get(len).unwrap()); // { source datum{len} source }

                            match self.sexpr(state) {
                                Ok(Some(())) => len += 1,
                                Ok(None) =>
                                    break Err(Error {
                                        what: ErrorWhat::Eof,
                                        at: Loc {
                                            source: state.get(len).unwrap(),
                                            pos: self.lexer.pos()
                                        }
                                    }),
                                res @ Err(_) => break res
                            }
                        },
                        None =>
                            break Err(Error {
                                what: ErrorWhat::Eof,
                                at: Loc { source: state.get(len).unwrap(), pos: self.lexer.pos() }
                            }),
                    }
                };

                match res {
                    Ok(_) => {
                        state.vector(len.try_into().expect("Vector too big to parse")); // { source datum }
                        let loc = Loc { source: state.remove(1).unwrap(), pos }; // { datum }
                        state.push_syntax(loc).unwrap(); // { syntax }
                    },
                    Err(_) =>
                        for _ in 0..len + 1 {
                            state.pop::<Value>().unwrap().unwrap();
                        }, // { }
                }

                res
            },
            Some(Ok((pos, Quote))) => {
                let _ = self.lexer.next(state);
                state.dup(); // { source source }
                match self.sexpr(state) {
                    Ok(Some(())) => {},
                    Ok(None) =>
                        return Err(Error {
                            what: ErrorWhat::Eof,
                            at: Loc {
                                source: state.pop().unwrap().unwrap(),
                                pos: self.lexer.pos()
                            }
                        }),
                    res @ Err(_) => {
                        state.pop::<Value>().unwrap().unwrap();
                        return res;
                    }
                } // { source datum }
                state.push_symbol("quote"); // { source datum 'quote }
                let loc = Loc { source: state.get(2).unwrap(), pos };
                state.push_syntax(loc).unwrap(); // { source datum #'quote }
                state.swap(); // { source 'quote datum }
                state.push(Value::NIL); // { source 'quote datum '() }
                state.cons();
                state.cons();
                let loc = Loc { source: state.remove(1).unwrap(), pos }; // { datum }
                state.push_syntax(loc).unwrap(); // { syntax }
                Ok(Some(()))
            },
            Some(Ok((_, Identifier(_)))) =>
                if let Some(Ok((pos, Identifier(sym)))) = self.lexer.next(state) {
                    let loc = Loc { source: state.pop().unwrap().unwrap(), pos };
                    state.push(sym);
                    state.push_syntax(loc).unwrap();
                    Ok(Some(()))
                } else {
                    unreachable!()
                },
            Some(Ok((_, Const(_)))) =>
                if let Some(Ok((pos, Const(v)))) = self.lexer.next(state) {
                    let loc = Loc { source: state.pop().unwrap().unwrap(), pos };
                    state.push(v);
                    state.push_syntax(loc).unwrap();
                    Ok(Some(()))
                } else {
                    unreachable!()
                },
            Some(Ok((pos, tok @ RParen))) => Err(Error {
                what: ErrorWhat::Unexpected(tok),
                at: Loc { source: state.pop().unwrap().unwrap(), pos }
            }),
            Some(Ok((pos, tok @ Dot))) => Err(Error {
                what: ErrorWhat::Unexpected(tok),
                at: Loc { source: state.pop().unwrap().unwrap(), pos }
            }),
            Some(Err(lex_err)) => Err(Error::from_lex(lex_err, state.pop().unwrap().unwrap())),
            None => {
                state.pop::<Value>().unwrap().unwrap();
                Ok(None)
            }
        }
    }

    pub unsafe fn sexprs(&mut self, state: &mut State, source: &str) -> Result<(), Error> {
        let mut nonempty = false;
        state.push_string(source); // { source }
        state.dup(); // { source source }

        loop {
            // { source (data prev)? source }
            match self.sexpr(state) {
                Ok(Some(())) => {
                    // { source (data prev)? datum }
                    let mut pair = Pair::new(state).unwrap_or_else(|| {
                        state.collect_garbage();
                        Pair::new(state).unwrap()
                    });

                    let parsed: Value = state.pop().unwrap().unwrap(); // { source (data prev)? }
                    if nonempty {
                        // { source data prev }
                        let mut prev = Pair::unchecked_downcast(state.pop().unwrap().unwrap());
                        prev.cdr = pair.into();
                    } else {
                        // { source }
                        state.push(pair);
                        nonempty = true;
                    } // { source data }
                    pair.car = parsed;
                    state.push(pair); // { source data prev }
                    state.push(state.get(2).unwrap()); // { source data prev source }
                },
                Ok(None) => {
                    if nonempty {
                        // { source data prev }
                        let mut pair = Pair::unchecked_downcast(state.pop().unwrap().unwrap());
                        pair.cdr = Value::NIL;
                    } else {
                        // { source }
                        state.push(Value::NIL);
                    } // { source data }
                    state.push_symbol("begin"); // { source data 'begin }
                    let loc = Loc { source: state.get(2).unwrap(), pos: Pos::default() };
                    state.push_syntax(loc).unwrap(); // { source data #'begin }
                    state.swap();
                    state.cons();
                    let loc = Loc { source: state.remove(1).unwrap(), pos: Pos::default() }; // { data }
                    state.push_syntax(loc).unwrap();
                    return Ok(());
                },
                Err(err) => {
                    if nonempty {
                        state.pop::<Value>().unwrap().unwrap(); // prev
                        state.pop::<Value>().unwrap().unwrap(); // data
                    }
                    state.pop::<Value>().unwrap().unwrap(); // source
                    return Err(err);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::{TryFrom, TryInto};

    use super::super::objects::{Symbol, Syntax, Vector};

    #[test]
    fn test_const() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ".chars()));

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();

        assert_eq!(parsed.datum, Value::from(23i16));
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" foo ".chars()));
        unsafe {
            state.push_symbol("foo");
        }
        let symbol: Symbol = state.pop().unwrap().unwrap();

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();
        let parsed: Symbol = state.downcast(parsed.datum).unwrap();

        assert_eq!(parsed.as_str(), symbol.as_str());
        assert_eq!(parsed.hash, symbol.hash);
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" () ".chars()));

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();

        assert_eq!(parsed.datum, Value::NIL);
    }

    #[test]
    fn test_proper() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5) ".chars()));

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();
        let parsed: Pair = state.downcast(parsed.datum).unwrap();

        assert_eq!(state.downcast::<Syntax>(parsed.car).unwrap().datum, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::NIL);
    }

    #[test]
    fn test_improper() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5 . 8) ".chars()));

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();
        let parsed: Pair = state.downcast(parsed.datum).unwrap();

        assert_eq!(state.downcast::<Syntax>(parsed.car).unwrap().datum, Value::from(5i16));
        assert_eq!(state.downcast::<Syntax>(parsed.cdr).unwrap().datum, Value::from(8i16));
    }

    #[test]
    fn test_vector() {
        let mut state = State::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" #(5) ".chars()));

        unsafe {
            state.push_string("test");
            parser.sexpr(&mut state).unwrap();
        }
        let parsed: Syntax = state.pop().unwrap().unwrap();
        let parsed: Vector = state.downcast(parsed.datum).unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(state.downcast::<Syntax>(parsed[0]).unwrap().datum, Value::from(5i16));
    }
}

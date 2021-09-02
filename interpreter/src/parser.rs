use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};

use super::lexer::{self, Lexer, Pos, Token};
use super::objects::{Pair, PgsString, Symbol, Syntax, Vector};
use super::refs::Value;

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
            ErrorWhat::Expected { expected, actual } => write!(f, "Was expecting '{}' but got '{}'.", expected, actual),
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

    unsafe fn list(&mut self, source: Value) -> Result<Value, Error> {
        use Token::*;

        match self.lexer.peek() {
            Some(Ok((_, RParen))) => Ok(Value::NIL),
            Some(Ok((_, Dot))) => {
                let _ = self.lexer.next();

                let cdr = self.sexpr(source)?;

                match self.lexer.peek() {
                    Some(Ok((_, RParen))) => {
                        let _ = self.lexer.next();
                        Ok(cdr)
                    },
                    Some(Ok((pos, tok))) => Err(Error {
                        what: ErrorWhat::Expected { expected: RParen, actual: tok },
                        at: Loc { source, pos }
                    }),
                    Some(Err(lex_err)) => Err(Error::from_lex(lex_err, source)),
                    None => Err(Error { what: ErrorWhat::Eof, at: Loc { source, pos: self.lexer.pos() } })
                }
            },
            Some(Ok(_)) => {
                let car = self.sexpr(source)?;
                let cdr = self.list(source)?;
                Ok(Pair::cons(car, cdr).into())
            },
            Some(Err(lex_err)) => Err(Error::from_lex(lex_err, source)),
            None => Err(Error { what: ErrorWhat::Eof, at: Loc { source, pos: self.lexer.pos() } })
        }
    }

    /// { source -- datum }
    unsafe fn sexpr(&mut self, source: Value) -> Result<Value, Error> {
        use Token::*;

        match self.lexer.peek() {
            Some(Ok((pos, LParen))) => {
                let _ = self.lexer.next();
                let ls = self.list(source)?;
                Ok(Syntax::new(ls, Value::FALSE, source, pos.line.try_into().unwrap(), pos.column.try_into().unwrap())
                    .into())
            },
            Some(Ok((pos, OpenVector))) => {
                let _ = self.lexer.next();
                let mut elems = Vec::new();

                loop {
                    match self.lexer.peek() {
                        Some(Ok((_, RParen))) => {
                            let _ = self.lexer.next();
                            break;
                        },
                        Some(Ok(_)) => {
                            let elem = self.sexpr(source)?;
                            elems.push(elem);
                        },
                        Some(Err(lex_err)) => return Err(Error::from_lex(lex_err, source)),
                        None => return Err(Error { what: ErrorWhat::Eof, at: Loc { source, pos: self.lexer.pos() } })
                    }
                }

                Ok(Syntax::new(
                    Vector::from_slice(&elems).into(),
                    Value::FALSE,
                    source,
                    pos.line.try_into().unwrap(),
                    pos.column.try_into().unwrap()
                )
                .into())
            },
            Some(Ok((pos, Quote))) => {
                let _ = self.lexer.next();
                let datum = self.sexpr(source)?;
                Ok(Syntax::new(
                    Pair::cons(
                        Syntax::new(
                            Symbol::new("quote").into(),
                            Value::FALSE,
                            source,
                            pos.line.try_into().unwrap(),
                            pos.column.try_into().unwrap()
                        )
                        .into(),
                        Pair::cons(datum, Value::NIL).into()
                    )
                    .into(),
                    Value::FALSE,
                    source,
                    pos.line.try_into().unwrap(),
                    pos.column.try_into().unwrap()
                )
                .into())
            },
            Some(Ok((_, Identifier(_)))) =>
                if let Some(Ok((pos, Identifier(sym)))) = self.lexer.next() {
                    Ok(Syntax::new(
                        sym.into(),
                        Value::FALSE,
                        source,
                        pos.line.try_into().unwrap(),
                        pos.column.try_into().unwrap()
                    )
                    .into())
                } else {
                    unreachable!()
                },
            Some(Ok((_, Const(_)))) =>
                if let Some(Ok((pos, Const(v)))) = self.lexer.next() {
                    Ok(Syntax::new(
                        v,
                        Value::FALSE,
                        source,
                        pos.line.try_into().unwrap(),
                        pos.column.try_into().unwrap()
                    )
                    .into())
                } else {
                    unreachable!()
                },
            Some(Ok((pos, tok @ RParen))) => Err(Error { what: ErrorWhat::Unexpected(tok), at: Loc { source, pos } }),
            Some(Ok((pos, tok @ Dot))) => Err(Error { what: ErrorWhat::Unexpected(tok), at: Loc { source, pos } }),
            Some(Err(lex_err)) => Err(Error::from_lex(lex_err, source)),
            None => Err(Error { what: ErrorWhat::Eof, at: Loc { source, pos: self.lexer.pos() } })
        }
    }

    pub unsafe fn sexprs(&mut self, source: &str) -> Result<Value, Error> {
        unsafe fn read_sexprs<I: Iterator<Item = char>>(this: &mut Parser<I>, source: Value) -> Result<Value, Error> {
            match this.lexer.peek() {
                Some(_) => {
                    let car = this.sexpr(source)?;
                    let cdr = read_sexprs(this, source)?;
                    Ok(Pair::cons(car, cdr).into())
                },
                None => Ok(Value::NIL)
            }
        }

        let source = PgsString::new(source).into();
        let ls = read_sexprs(self, source)?;
        let pos = Pos::default();
        Ok(Syntax::new(
            Pair::cons(Symbol::new("begin").into(), ls).into(),
            Value::FALSE,
            source,
            pos.line.try_into().unwrap(),
            pos.column.try_into().unwrap()
        )
        .into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::interpreter::Interpreter;
    use super::super::objects::{Symbol, Syntax, Vector};

    use std::convert::TryFrom;

    #[test]
    fn test_const() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();

        assert_eq!(parsed.datum, Value::from(23i16));
    }

    #[test]
    fn test_symbol() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let symbol: Symbol = unsafe { Symbol::new("foo") };
        let mut parser = Parser::new(Lexer::new(" foo ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();
        let parsed: Symbol = parsed.datum.try_into().unwrap();

        assert_eq!(parsed.as_str(), symbol.as_str());
        assert_eq!(parsed.hash, symbol.hash);
    }

    #[test]
    fn test_nil() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" () ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();

        assert_eq!(parsed.datum, Value::NIL);
    }

    #[test]
    fn test_proper() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5) ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();
        let parsed: Pair = parsed.datum.try_into().unwrap();

        assert_eq!(Syntax::try_from(parsed.car).unwrap().datum, Value::from(5i16));
        assert_eq!(parsed.cdr, Value::NIL);
    }

    #[test]
    fn test_improper() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5 . 8) ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();
        let parsed: Pair = parsed.datum.try_into().unwrap();

        assert_eq!(Syntax::try_from(parsed.car).unwrap().datum, Value::from(5i16));
        assert_eq!(Syntax::try_from(parsed.cdr).unwrap().datum, Value::from(8i16));
    }

    #[test]
    fn test_vector() {
        let _ = Interpreter::new(&[], 1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" #(5) ".chars()));

        let parsed = unsafe { parser.sexpr(PgsString::new("test").into()).unwrap() };
        let parsed: Syntax = parsed.try_into().unwrap();
        let parsed: Vector = parsed.datum.try_into().unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(Syntax::try_from(parsed[0]).unwrap().datum, Value::from(5i16));
    }
}

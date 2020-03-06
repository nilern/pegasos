use std::iter::Peekable;

use super::lexer::{Token, Lexer};
use super::state::State;
use super::value::{Value, HeapValue, Symbol, Pair, Vector};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Peekable<Lexer<'a>>) -> Self { Self {lexer} }

    pub fn sexpr(&mut self, state: &mut State) -> Result<(), ()> {
        use Token::*;

        match self.lexer.peek() {
            Some(LParen) => {
                let _ = self.lexer.next();
                let mut len = 0;
                
                loop {
                    match self.lexer.peek() {
                        Some(RParen) => {
                            let _ = self.lexer.next();

                            state.push(Value::NIL);
                            for _ in 0..len {
                                unsafe { state.cons(); }
                            }
                            return Ok(());
                        },
                        Some(Dot) => {
                            let _ = self.lexer.next();
                            self.sexpr(state)?;
                            if self.lexer.next() != Some(RParen) {
                                return Err(());
                            }

                            for _ in 0..len {
                                unsafe { state.cons(); }
                            }
                            return Ok(());
                        }
                        Some(_) => {
                            self.sexpr(state)?;
                            len += 1;
                        },
                        None => return Err(())
                    }
                }
            },
            Some(OpenVector) => {
                let _ = self.lexer.next();
                let mut len = 0;

                loop {
                    match self.lexer.peek() {
                        Some(RParen) => {
                            let _ = self.lexer.next();
                            unsafe { state.vector(len); }
                            return Ok(());
                        },
                        Some(_) => {
                            self.sexpr(state)?;
                            len += 1;
                        },
                        None => return Err(())
                    }
                }
            },
            Some(Quote) => {
                let _ = self.lexer.next();
                unsafe { state.push_symbol("quote") };
                self.sexpr(state)?;
                state.push(Value::NIL);
                unsafe {
                    state.cons();
                    state.cons();
                }
                return Ok(());
            },
            Some(Identifier(_)) => {
                if let Some(Identifier(cs)) = self.lexer.next() {
                    unsafe { state.push_symbol(cs); }
                    Ok(())
                } else {
                    unreachable!()
                }
            }
            Some(Const(_)) => {
                if let Some(Const(v)) = self.lexer.next() {
                    state.push(v);
                    Ok(())
                } else {
                    unreachable!()
                }
            },
            Some(_) => unimplemented!(),
            None => Err(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::{TryFrom, TryInto};

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ").peekable());
        parser.sexpr(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::try_from(23isize).unwrap());
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" foo ").peekable());
        unsafe { state.push_symbol("foo"); }
        let symbol: Symbol = state.pop().unwrap().try_into().unwrap();

        parser.sexpr(&mut state).unwrap();
        let parsed: Value = state.pop().unwrap();
        let parsed: HeapValue<()> = parsed.try_into().unwrap();
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

        assert_eq!(parsed.car, Value::try_from(5isize).unwrap());
        assert_eq!(parsed.cdr, Value::NIL);
    }

    #[test]
    fn test_improper() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" (5 . 8) ").peekable());

        parser.sexpr(&mut state).unwrap();
        let parsed: Pair = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.car, Value::try_from(5isize).unwrap());
        assert_eq!(parsed.cdr, Value::try_from(8isize).unwrap());
    }

    #[test]
    fn test_vector() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" #(5) ").peekable());

        parser.sexpr(&mut state).unwrap();
        let parsed: Vector = state.pop().unwrap().try_into().unwrap();

        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0], Value::try_from(5isize).unwrap());
    }
}


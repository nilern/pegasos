use std::iter::Peekable;

use super::lexer::{Token, Lexer};
use super::state::State;
use super::value::Value;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Peekable<Lexer<'a>>) -> Self { Self {lexer} }

    pub fn sexpr(&mut self, state: &mut State) -> Result<(), ()> {
        use Token::*;

        match self.lexer.peek() {
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

    use std::convert::TryFrom;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 16, 1 << 20);
        let mut parser = Parser::new(Lexer::new(" 23 ").peekable());
        parser.sexpr(&mut state);
        assert_eq!(state.pop(), Some(Value::try_from(23).unwrap()));
    }
}


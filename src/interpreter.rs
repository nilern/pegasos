use std::convert::{TryFrom, TryInto};

use super::state::State;
use super::value::{Value, UnpackedValue, UnpackedHeapValue, PgsString, Symbol, Pair};

pub fn eval(state: &mut State) -> Result<(), ()> {
    match state.peek().unwrap().unpack() {
        UnpackedValue::ORef(oref) => match oref.unpack() {
            UnpackedHeapValue::Pair(pair) => if let Ok(op) = Symbol::try_from(pair.car) {
                match op.as_str() {
                    "quote" => if let Ok(args) = Pair::try_from(pair.cdr) {
                        if args.cdr == Value::NIL {
                            state.pop();
                            state.push(args.car);
                            return Ok(());
                        } else {
                            return Err(())
                        }
                    } else {
                        return Err(())
                    },
                    _ => unimplemented!()
                }
            } else {
                unimplemented!()
            },
            UnpackedHeapValue::String(_) => return Ok(()),
            _ => unimplemented!()
        },
        UnpackedValue::Fixnum(_) => return Ok(()),
        UnpackedValue::Flonum(_) => return Ok(()),
        UnpackedValue::Char(_) => return Ok(()),
        UnpackedValue::Bool(_) => return Ok(()),
        UnpackedValue::Undefined => return Ok(()),
        UnpackedValue::Eof => return Ok(()),
        UnpackedValue::Nil => return Err(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 12, 1 << 20);
        
        state.push(Value::from('a'));
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from('a'));

        unsafe { state.push_string("foo") };
        eval(&mut state).unwrap();
        let res: PgsString = state.pop().unwrap().try_into().unwrap();

        assert_eq!(res.as_str(), "foo");
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(1 << 12, 1 << 20);
        
        state.push(Value::NIL);
        let res = eval(&mut state);

        assert!(res.is_err());
    }

    #[test]
    fn test_quote() {
        let mut state = State::new(1 << 12, 1 << 20);
        let mut parser = Parser::new(Lexer::new("'()").peekable());

        parser.sexpr(&mut state).unwrap();
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())").peekable());

        parser.sexpr(&mut state).unwrap();
        assert!(eval(&mut state).is_err());
    }
}


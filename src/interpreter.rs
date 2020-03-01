use super::state::State;
use super::value::{Value, UnpackedValue};

pub fn eval(state: &mut State) -> Result<(), ()> {
    match state.peek().unwrap().unpack() {
        UnpackedValue::Fixnum(_) => return Ok(()),
        UnpackedValue::Flonum(_) => return Ok(()),
        UnpackedValue::Char(_) => return Ok(()),
        UnpackedValue::Bool(_) => return Ok(()),
        UnpackedValue::Undefined => return Ok(()),
        UnpackedValue::Eof => return Ok(()),
        UnpackedValue::Nil => return Err(()),
        _ => unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const() {
        let mut state = State::new(1 << 12, 1 << 20);
        
        state.push(Value::from('a'));
        eval(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from('a'));
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(1 << 12, 1 << 20);
        
        state.push(Value::NIL);
        let res = eval(&mut state);

        assert!(res.is_err());
    }
}


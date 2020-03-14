use std::convert::{TryFrom, TryInto};

use super::error::PgsError;
use super::interpreter::{Op, Primitive, RuntimeError};
use super::objects::Symbol;
use super::refs::{HeapValue, Value};
use super::state::State;

pub const PRIMITIVES: [(&str, Primitive); 7] = [
    ("null?", is_null),
    ("symbol?", is_symbol),
    ("%length", object_length),
    ("%slot-ref", slot_ref),
    ("%slot-set!", slot_set),
    ("make-vector", make_vector),
    ("fx<?", fx_lt)
];

fn is_null(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 1 {
        let v = state.pop().unwrap();
        state.pop().unwrap(); // callee
        state.push(v == Value::NIL);
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (1, false), got: argc }
            .into())
    }
}

fn is_symbol(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 1 {
        let v = state.pop().unwrap();
        state.pop().unwrap(); // callee
        state.push(Symbol::try_from(v).is_ok());
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (1, false), got: argc }
            .into())
    }
}

fn object_length(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 1 {
        if let Ok(oref) = HeapValue::<()>::try_from(state.pop().unwrap()) {
            state.pop().unwrap(); // callee
            state.push(Value::try_from(unsafe { (*oref.as_ptr()).len() }).unwrap());
            state.push(1u16);
            Ok(Op::Continue)
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (1, false), got: argc }
            .into())
    }
}

fn slot_ref(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 2 {
        if let Ok(i) = <Value as TryInto<usize>>::try_into(state.pop().unwrap()) {
            if let Ok(oref) = HeapValue::<()>::try_from(state.pop().unwrap()) {
                state.pop().unwrap(); // callee
                state.push(*oref.slots().get(i).unwrap());
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (2, false), got: argc }
            .into())
    }
}

fn slot_set(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 3 {
        let v = state.pop().unwrap();

        if let Ok(i) = <Value as TryInto<usize>>::try_into(state.pop().unwrap()) {
            if let Ok(mut oref) = HeapValue::<()>::try_from(state.pop().unwrap()) {
                state.pop().unwrap(); // callee
                *oref.slots_mut().get_mut(i).unwrap() = v;
                state.push(Value::UNSPECIFIED);
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (2, false), got: argc }
            .into())
    }
}

fn make_vector(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 1 {
        if let Ok(len) = state.pop().unwrap().try_into() {
            state.pop().unwrap(); // callee
            unsafe { state.push_vector(len) };
            state.push(1u16);
            Ok(Op::Continue)
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (1, false), got: argc }
            .into())
    }
}

fn fx_lt(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 2 {
        if let Ok(a) = state.pop().unwrap().try_into() {
            if let Ok(b) = state.pop().unwrap().try_into() {
                state.pop().unwrap(); // callee
                state.push(isize::lt(&b, &a)); // OPTIMIZE
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (2, false), got: argc }
            .into())
    }
}


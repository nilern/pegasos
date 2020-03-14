use std::convert::{TryFrom, TryInto};

use super::error::PgsError;
use super::interpreter::{Op, Primitive, RuntimeError};
use super::objects::{Cars, Closure, Pair, Symbol};
use super::refs::{HeapValue, Value};
use super::state::State;

pub const PRIMITIVES: [(&str, Primitive); 14] = [
    ("eq?", eq),
    ("apply", apply),
    ("null?", is_null),
    ("symbol?", is_symbol),
    ("%length", object_length),
    ("%slot-ref", slot_ref),
    ("%slot-set!", slot_set),
    ("%record", record),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr),
    ("make-vector", make_vector),
    ("fx<?", fx_lt),
    ("fx+", fx_add)
];

macro_rules! count {
    () => (0);
    ($param:ident) => (1);
    ($param:ident, $($params:ident),*) => (1 + count!($($params),*));
}

macro_rules! pop_params {
    ($state:ident) => {};
    ($state:ident, $param:ident) => {let $param = $state.pop().unwrap();};
    ($state:ident, $param:ident, $($params:ident),*) => {
        pop_params!($state, $($params),*);
        let $param = $state.pop().unwrap();
    };
}

macro_rules! checked_primitive_body {
    (() $body:block) => {$body};
    (($param:ident : $typ:ty) $body:block) => {
        if let Ok($param) = <Value as TryInto<$typ>>::try_into($param) {
            checked_primitive_body!(() $body)
        } else {
            unimplemented!()
        }
    };
    (($param:ident : $typ:ty, $($params:ident : $typs:ty),*) $body:block) => {
        if let Ok($param) = <Value as TryInto<$typ>>::try_into($param) {
            checked_primitive_body!(($($params : $typs),*) $body)
        } else {
            unimplemented!()
        }
    };
}

macro_rules! primitive {
    ($name:ident $state:ident ($($params:ident : $typs:ty),*) $body:block) => {
        fn $name($state: &mut State) -> Result<Op, PgsError> {
            let arity = count!($($params),*);
            let argc: usize = $state.pop().unwrap().try_into().unwrap();

            if argc == arity {
                pop_params!($state, $($params),*);
                $state.pop().unwrap(); // callee
                checked_primitive_body!(($($params : $typs),*) $body)
            } else {
                Err(RuntimeError::Argc { callee: $state.get(argc).unwrap(), params: (arity, false), got: argc }
                   .into())
            }
        }
    };
}

primitive! { eq state (a: Value, b: Value) {
    state.push(a == b);
    state.push(1u16);
    Ok(Op::Continue)
}}

fn apply(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc >= 2 {
        if Closure::try_from(state.get(argc - 1).unwrap()).is_ok() {
            let ls = state.pop().unwrap();
            let mut final_argc = argc - 2;
            let mut cars = Cars::of(ls);

            for arg in &mut cars {
                state.push(arg);
                final_argc += 1;
            }

            if cars.remainder() == Value::NIL {
                state.remove(final_argc + 1).unwrap(); // callee
                state.push(<usize as TryInto<Value>>::try_into(final_argc).unwrap());
                Ok(Op::Apply)
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (2, true), got: argc }
            .into())
    }
}

primitive! { is_null state (v: Value) {
    state.push(v == Value::NIL);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { is_symbol state (v: Value) {
    state.push(Symbol::try_from(v).is_ok());
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { object_length state (v: HeapValue<()>) {
    state.push(Value::try_from(unsafe { (*v.as_ptr()).len() }).unwrap());
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { slot_ref state (o: HeapValue<()>, i: usize) {
    state.push(*o.slots().get(i).unwrap());
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { slot_set state (o: HeapValue<()>, i: usize, v: Value) {
    let mut o = o;
    *o.slots_mut().get_mut(i).unwrap() = v;
    state.push(Value::UNSPECIFIED);
    state.push(1u16);
    Ok(Op::Continue)
}}

fn cons(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc == 2 {
        unsafe {
            state.cons();
        }
        state.remove(1).unwrap(); // callee
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (2, false), got: argc }
            .into())
    }
}

primitive! { car state (ls: Pair) {
    state.push(ls.car);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { cdr state (ls: Pair) {
    state.push(ls.cdr);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { make_vector state (len: usize) {
    unsafe { state.push_vector(len) };
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_lt state (a: isize, b: isize) {
    state.push(a < b); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_add state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a + b).unwrap()); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

fn record(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc > 0 {
        unsafe {
            state.record(argc);
        }
        state.remove(1).unwrap(); // callee
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (1, true), got: argc }
            .into())
    }
}

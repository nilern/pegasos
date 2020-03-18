use std::convert::{TryFrom, TryInto};
use std::io;
use std::mem::transmute;
use std::ptr;

use super::error::PgsError;
use super::interpreter::{Op, Primitive, RuntimeError};
use super::objects::{Cars, Closure, Pair, Symbol, Vector};
use super::refs::{FrameTag, HeapValue, Tag, Value};
use super::state::State;

pub const PRIMITIVES: [(&str, Primitive); 27] = [
    ("void", void),
    ("eq?", eq),
    ("%identity-hash", identity_hash),
    ("apply", apply),
    ("call-with-values", call_with_values),
    ("values", values),
    ("null?", is_null),
    ("symbol?", is_symbol),
    ("%symbol-hash", symbol_hash),
    ("%immediate-type-index", immediate_tag),
    ("%heap-type-index", heap_tag),
    ("%length", object_length),
    ("%slot-ref", slot_ref),
    ("%slot-set!", slot_set),
    ("%record", record),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr),
    ("make-vector", make_vector),
    ("vector-copy!", vector_copy),
    ("fx<?", fx_lt),
    ("fx+", fx_add),
    ("fx-", fx_sub),
    ("bitwise-and", bitwise_and),
    ("bitwise-or", bitwise_or),
    ("arithmetic-shift", arithmetic_shift),
    ("bit-count", bit_count)
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
                pop_params!($state $(,$params)*);
                $state.pop().unwrap(); // callee
                checked_primitive_body!(($($params : $typs),*) $body)
            } else {
                Err(RuntimeError::Argc { callee: $state.get(argc).unwrap(), params: (arity, false), got: argc }
                   .into())
            }
        }
    };
}

primitive! { void state () {
    state.push(Value::UNSPECIFIED);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { eq state (a: Value, b: Value) {
    state.push(a == b);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { identity_hash state (v: Value) {
    state.push(unsafe { transmute::<usize, Value>(v.identity_hash() << Value::SHIFT | Tag::Fixnum as usize) });
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

primitive! { call_with_values state (producer: Value, consumer: Value) {
    state.push(consumer);
    state.push_env();
    state.push(FrameTag::CallWithValues);
    state.push(producer);
    state.push(0u16);
    Ok(Op::Apply)
}}

fn values(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.peek().unwrap().try_into().unwrap();
    state.remove(argc + 1).unwrap(); // callee
    Ok(Op::Continue)
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

primitive! { symbol_hash state (v: Symbol) {
    state.push(unsafe { transmute::<usize, Value>((v.hash as usize) << Value::SHIFT | Tag::Fixnum as usize) });
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { immediate_tag state (v: Value) {
    state.push(v.immediate_type_index() as u16);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { heap_tag state (v: HeapValue<()>) {
    state.push(v.heap_tag() as u16);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { object_length state (v: HeapValue<()>) {
    state.push(Value::try_from(unsafe { (*v.as_ptr()).len() }).unwrap());
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { slot_ref state (o: HeapValue<()>, i: isize) {
    if let Some(&v) = o.slots().get(i as usize) {
        state.push(v);
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Bounds { value: o.into(), index: i, len: o.slots().len() }.into())
    }
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

fn make_vector(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    match argc {
        1 =>
            if let Ok(len) = <Value as TryInto<usize>>::try_into(state.pop().unwrap()) {
                unsafe {
                    state.push_vector(len);
                }
            } else {
                unimplemented!()
            },
        2 => {
            let v = state.pop().unwrap();
            if let Ok(len) = <Value as TryInto<usize>>::try_into(state.pop().unwrap()) {
                for _ in 0..len {
                    state.push(v);
                }
                unsafe {
                    state.vector(len);
                }
            } else {
                unimplemented!()
            }
        },
        argc =>
        // TODO: arity is actually 1 | 2, not 2:
            return Err(RuntimeError::Argc {
                callee: state.get(argc).unwrap(),
                params: (2, false),
                got: argc
            }
            .into()),
    }

    state.remove(1).unwrap(); // callee
    state.push(1u16);
    Ok(Op::Continue)
}

primitive! { vector_copy state (to: Vector, at: isize, from: Vector, start: isize, end: isize) {
    assert!(0 <= at && at as usize <= to.len());
    assert!(0 <= start && start as usize <= from.len());
    assert!(start <= end && end as usize <= from.len());

    unsafe { ptr::copy((from.data() as *const Value).offset(start), (to.data() as *mut Value).offset(at), (end - start) as usize); }
    state.push(Value::UNSPECIFIED);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_lt state (a: isize, b: isize) {
    state.push(a < b); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_add state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a.checked_add(b).expect("overflow")).unwrap()); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

fn fx_sub(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    match argc {
        1 => {
            let a: isize = state.pop().unwrap().try_into().unwrap();
            state.pop().unwrap(); // callee
            state.push(Value::try_from(a.checked_neg().unwrap()).unwrap()); // OPTIMIZE
            state.push(1u16);
            Ok(Op::Continue)
        },
        2 => {
            let b: isize = state.pop().unwrap().try_into().unwrap();
            let a: isize = state.pop().unwrap().try_into().unwrap();
            state.pop().unwrap(); // callee
            state.push(
                <isize as TryInto<Value>>::try_into(a.checked_sub(b).expect("overflow")).unwrap()
            ); // OPTIMIZE
            state.push(1u16);
            Ok(Op::Continue)
        },
        _ =>
        // FIXME: arity is actually 1 | 2
            Err(RuntimeError::Argc {
                callee: state.get(argc).unwrap(),
                params: (2, false),
                got: argc
            }
            .into()),
    }
}

primitive! { bitwise_and state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a & b).unwrap()); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_or state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a | b).unwrap()); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { arithmetic_shift state (n: isize, count: isize) {
    let shifted = if count < 0 {
        n.checked_shr((-count) as u32).expect("overflow")
    } else {
        n.checked_shl(count as u32).expect("overflow")
    };
    state.push(<isize as TryInto<Value>>::try_into(shifted).unwrap()); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bit_count state (n: isize) {
    state.push(n.count_ones() as u16);
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

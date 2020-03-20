use std::convert::{TryFrom, TryInto};
use std::mem::transmute;
use std::ptr;

use super::error::PgsError;
use super::interpreter::{Op, RuntimeError};
use super::objects::{Cars, Closure, Pair, Symbol, Syntax, Vector};
use super::refs::{FrameTag, HeapValue, Primop, Tag, Value};
use super::state::State;

pub fn perform(op: Primop, state: &mut State) -> Result<Op, PgsError> {
    use Primop::*;

    match op {
        Void => void(state),
        Eq => eq(state),
        IdentityHash => identity_hash(state),
        Call => call(state),
        Apply => apply(state),
        CallWithValues => call_with_values(state),
        Values => values(state),
        SymbolHash => symbol_hash(state),
        ImmediateTypeIndex => immediate_tag(state),
        HeapTypeIndex => heap_tag(state),
        Length => object_length(state),
        SlotRef => slot_ref(state),
        SlotSet => slot_set(state),
        Record => record(state),
        Cons => cons(state),
        Car => car(state),
        Cdr => cdr(state),
        MakeVector => make_vector(state),
        VectorCopy => vector_copy(state),
        FxLt => fx_lt(state),
        FxAdd => fx_add(state),
        FxSub => fx_sub(state),
        FxMul => fx_mul(state),
        BitwiseAnd => bitwise_and(state),
        BitwiseIor => bitwise_ior(state),
        BitwiseXor => bitwise_xor(state),
        ArithmeticShift => arithmetic_shift(state),
        BitCount => bit_count(state),
        MakeSyntax => make_syntax(state)
    }
}

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

fn call(state: &mut State) -> Result<Op, PgsError> {
    let arg_count: usize = state.pop().unwrap().try_into().unwrap();
    let callee: Closure = unsafe { transmute::<Value, Closure>(state.get(arg_count).unwrap()) };

    match callee.clovers() {
        &[env, body, rest_param, params] => {
            let params: Vector = params.try_into().unwrap();
            let fixed_argc = params.len();
            let variadic = rest_param != Value::NIL;

            // OPTIMIZE?
            if arg_count == fixed_argc || variadic && arg_count >= fixed_argc {
                // FIXME: It is an error for a <variable> to appear more than
                // once
                let leftover_argc = arg_count - fixed_argc;

                state.remove(arg_count); // callee
                state.insert_after(arg_count, body);
                state.insert_after(arg_count, params.into());
                state.insert_after(arg_count, rest_param);

                state.set_env(env.try_into().unwrap());
                unsafe { state.push_scope() };

                if variadic {
                    state.push(Value::NIL);
                    for _ in 0..leftover_argc {
                        unsafe { state.cons() };
                    }
                    let rest_param = state.get(fixed_argc + 1).unwrap();
                    state.insert_after(1, rest_param);
                    unsafe { state.define() };
                }

                for ri in 0..fixed_argc {
                    let i = fixed_argc - 1 - ri;
                    let params = unsafe { transmute::<Value, Vector>(state.get(i + 2).unwrap()) };
                    state.insert_after(1, params[i]);
                    unsafe { state.define() };
                }

                state.pop(); // rest_param
                state.pop(); // params
                Ok(Op::Eval)
            } else {
                state.raise(RuntimeError::Argc {
                    callee: callee.into(),
                    params: (fixed_argc, variadic),
                    got: arg_count
                })
            }
        },
        _ => unreachable!()
    }
}

fn apply(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    if argc >= 2 {
        let f: Closure = state.get(argc - 1).unwrap().try_into()?;
        let ls = state.pop().unwrap();
        let mut final_argc = argc - 2;
        let mut cars = Cars::of(ls);

        for arg in &mut cars {
            state.push(arg);
            final_argc += 1;
        }

        if cars.remainder() == Value::NIL {
            state.remove(final_argc + 1).unwrap(); // callee
            state.push(<usize as TryInto<Value>>::try_into(final_argc)?);
            Ok(Op::Apply)
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

primitive! { symbol_hash state (v: Symbol) {
    state.push(unsafe { transmute::<usize, Value>((v.hash as usize) << Value::SHIFT | Tag::Fixnum as usize) });
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { immediate_tag state (v: Value) {
    state.push(v.tag() as u16);
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
        1 => {
            let len: usize = state.pop().unwrap().try_into()?;
            unsafe { state.push_vector(len) };
        },
        2 => {
            let v = state.pop().unwrap();
            let len: usize = state.pop().unwrap().try_into()?;
            for _ in 0..len {
                state.push(v);
            }
            unsafe { state.vector(len) };
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
    state.push(<isize as TryInto<Value>>::try_into(a.checked_add(b).expect("overflow"))?); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

fn fx_sub(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap().try_into().unwrap();

    match argc {
        1 => {
            let a: isize = state.pop().unwrap().try_into()?;
            state.pop().unwrap(); // callee
            state.push(Value::try_from(a.checked_neg().unwrap())?); // OPTIMIZE
            state.push(1u16);
            Ok(Op::Continue)
        },
        2 => {
            let b: isize = state.pop().unwrap().try_into()?;
            let a: isize = state.pop().unwrap().try_into()?;
            state.pop().unwrap(); // callee
            state.push(<isize as TryInto<Value>>::try_into(a.checked_sub(b).expect("overflow"))?); // OPTIMIZE
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

primitive! { fx_mul state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a.checked_mul(b).expect("overflow"))?); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_and state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a & b)?); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_ior state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a | b)?); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_xor state (a: isize, b: isize) {
    state.push(<isize as TryInto<Value>>::try_into(a ^ b)?); // OPTIMIZE
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { arithmetic_shift state (n: isize, count: isize) {
    let shifted = if count < 0 {
        n.checked_shr((-count) as u32).expect("overflow")
    } else {
        n.checked_shl(count as u32).expect("overflow")
    };
    state.push(<isize as TryInto<Value>>::try_into(shifted)?); // OPTIMIZE
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

primitive! { make_syntax state (datum: Value, scopes: Value, source: Value, line: Value, column: Value) {
    let syntax = Syntax::new(state, datum, scopes, source, line, column).unwrap_or_else(|| {
        state.push(datum);
        state.push(scopes);
        state.push(source);
        state.push(line);
        state.push(column);
        unsafe { state.collect_garbage(); }
        let column = state.pop().unwrap();
        let line = state.pop().unwrap();
        let source = state.pop().unwrap();
        let scopes = state.pop().unwrap();
        let datum = state.pop().unwrap();
        Syntax::new(state, datum, scopes, source, line, column).unwrap()
    });
    state.push(syntax);
    state.push(1u16);
    Ok(Op::Continue)
}}

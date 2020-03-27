use std::convert::{TryFrom, TryInto};
use std::mem::transmute;

use super::error::PgsError;
use super::interpreter::{Op, RuntimeError};
use super::objects::{Closure, Pair, Symbol, Syntax, Vector};
use super::refs::{Fixnum, FrameTag, HeapValue, Primop, Tag, Value};
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
        Tag => tag(state),
        Length => object_length(state),
        SlotRef => slot_ref(state),
        SlotSet => slot_set(state),
        Make => make(state),
        Cons => cons(state),
        Car => car(state),
        Cdr => cdr(state),
        MakeVector => make_vector(state),
        FxLt => fx_lt(state),
        FxAdd => fx_add(state),
        FxSub => fx_sub(state),
        FxMul => fx_mul(state),
        BitwiseAnd => bitwise_and(state),
        BitwiseIor => bitwise_ior(state),
        BitwiseXor => bitwise_xor(state),
        FxArithmeticShift => fx_arithmetic_shift(state),
        BitCount => bit_count(state),
        MakeSyntax => make_syntax(state),
        MakeType => make_type(state),
        Type => typ(state)
    }
}

macro_rules! count {
    () => (0);
    ($param:ident) => (1);
    ($param:ident, $($params:ident),*) => (1 + count!($($params),*));
}

macro_rules! pop_params {
    ($state:ident) => {};
    ($state:ident, $param:ident : $typ:ty) => {let $param = $state.pop::<$typ>().unwrap()?;};
    ($state:ident, $param:ident : $typ:ty, $($params:ident : $typs:ty),*) => {
        pop_params!($state, $($params : $typs),*);
        let $param = $state.pop::<$typ>().unwrap()?;
    };
}

macro_rules! primitive {
    ($name:ident $state:ident ($($params:ident : $typs:ty),*) $body:block) => {
        fn $name($state: &mut State) -> Result<Op, PgsError> {
            let arity = count!($($params),*);
            let argc: usize = $state.pop().unwrap()?;

            if argc == arity {
                pop_params!($state $(,$params : $typs)*);
                $state.pop::<Value>().unwrap().unwrap(); // callee
                $body
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
    let arg_count: usize = state.pop().unwrap()?;
    let callee: Closure = unsafe { transmute::<Value, Closure>(state.get(arg_count).unwrap()) };

    match callee.clovers() {
        &[env, body, rest_param, params] => {
            let params = state.downcast::<Vector>(params).unwrap();
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

                state.set_env(state.downcast(env)?);
                unsafe { state.push_scope() };

                if variadic {
                    state.push(Value::NIL);
                    for _ in 0..leftover_argc {
                        unsafe { state.cons() };
                    }
                    let rest_param = state.get(fixed_argc + 1).unwrap();
                    state.insert_after(1, rest_param);
                    unsafe { state.define()? };
                }

                for ri in 0..fixed_argc {
                    let i = fixed_argc - 1 - ri;
                    let params = unsafe { transmute::<Value, Vector>(state.get(i + 2).unwrap()) };
                    state.insert_after(1, params[i]);
                    unsafe { state.define()? };
                }

                state.pop::<Value>().unwrap().unwrap(); // rest_param
                state.pop::<Value>().unwrap().unwrap(); // params
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
    let argc: usize = state.pop().unwrap()?;

    if argc >= 2 {
        let mut ls: Value = state.pop::<Value>().unwrap().unwrap();
        let mut final_argc = argc - 2;

        while let Ok(pair) = state.downcast::<Pair>(ls) {
            state.push(pair.car);
            final_argc += 1;
            ls = pair.cdr;
        }

        if ls == Value::NIL {
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
    let argc = state.peek().unwrap();
    let argc: usize = state.downcast(argc)?;
    state.remove(argc + 1).unwrap(); // callee
    Ok(Op::Continue)
}

primitive! { symbol_hash state (v: Symbol) {
    state.push(unsafe { transmute::<usize, Value>((v.hash as usize) << Value::SHIFT | Tag::Fixnum as usize) });
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { tag state (v: Value) {
    state.push(v.tag() as u16);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { object_length state (v: HeapValue<()>) {
    state.push(Value::try_from(unsafe { (*v.header()).len() }).unwrap());
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { slot_ref state (o: HeapValue<()>, i: Fixnum) {
    if let Some(&v) = o.slots().get(<Fixnum as Into<usize>>::into(i)) {
        state.push(v);
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Bounds { value: o.into(), index: i, len: o.slots().len() }.into())
    }
}}

primitive! { slot_set state (o: HeapValue<()>, i: Fixnum, v: Value) {
    let mut o = o;
    *o.slots_mut().get_mut(<Fixnum as Into<usize>>::into(i)).unwrap() = v;
    state.push(Value::UNSPECIFIED);
    state.push(1u16);
    Ok(Op::Continue)
}}

fn cons(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap()?;

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
    let argc: usize = state.pop().unwrap()?;

    match argc {
        1 => {
            let len: Fixnum = state.pop().unwrap()?;
            unsafe { state.push_vector(len) };
        },
        2 => {
            let v: Value = state.pop::<Value>().unwrap().unwrap();
            let len: Fixnum = state.pop().unwrap()?;
            for _ in 0..<Fixnum as Into<usize>>::into(len) {
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

primitive! { fx_lt state (a: Fixnum, b: Fixnum) {
    state.push(a < b);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_add state (a: Fixnum, b: Fixnum) {
    state.push((a + b)?);
    state.push(1u16);
    Ok(Op::Continue)
}}

fn fx_sub(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap()?;

    match argc {
        1 => {
            let a: Fixnum = state.pop().unwrap()?;
            state.pop::<Value>().unwrap().unwrap(); // callee
            state.push((-a)?);
            state.push(1u16);
            Ok(Op::Continue)
        },
        2 => {
            let b: Fixnum = state.pop().unwrap()?;
            let a: Fixnum = state.pop().unwrap()?;
            state.pop::<Value>().unwrap().unwrap(); // callee
            state.push((a - b)?);
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

primitive! { fx_mul state (a: Fixnum, b: Fixnum) {
    state.push((a * b)?);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_and state (a: Fixnum, b: Fixnum) {
    state.push(a & b);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_ior state (a: Fixnum, b: Fixnum) {
    state.push(a | b);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bitwise_xor state (a: Fixnum, b: Fixnum) {
    state.push(a ^ b);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { fx_arithmetic_shift state (n: Fixnum, count: Fixnum) {
    let shifted = if count < 0u16.into() {
        n >> (-count)?
    } else {
        n << count
    };
    state.push(shifted?);
    state.push(1u16);
    Ok(Op::Continue)
}}

primitive! { bit_count state (n: Fixnum) {
    state.push(n.count_ones());
    state.push(1u16);
    Ok(Op::Continue)
}}

fn make(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap()?;

    if argc > 0 {
        unsafe { state.make(argc - 1) };
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
        let column = state.pop().unwrap().unwrap();
        let line = state.pop().unwrap().unwrap();
        let source = state.pop().unwrap().unwrap();
        let scopes = state.pop().unwrap().unwrap();
        let datum = state.pop().unwrap().unwrap();
        Syntax::new(state, datum, scopes, source, line, column).unwrap()
    });
    state.push(syntax);
    state.push(1u16);
    Ok(Op::Continue)
}}

fn make_type(state: &mut State) -> Result<Op, PgsError> {
    let argc: usize = state.pop().unwrap()?;

    if argc >= 4 {
        unsafe { state.make_type(argc - 4)? };
        state.remove(1).unwrap(); // callee
        state.push(1u16);
        Ok(Op::Continue)
    } else {
        Err(RuntimeError::Argc { callee: state.get(argc).unwrap(), params: (4, true), got: argc }
            .into())
    }
}

primitive! { typ state (v: Value) {
    let typ = state.type_of(v);
    state.push(typ);
    state.push(1u16);
    Ok(Op::Continue)
}}

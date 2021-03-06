use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Formatter};
use std::fs;
use std::io;
use std::mem::transmute;

use super::error::PgsError;
use super::lexer::Lexer;
use super::objects::{Bindings, Closure, Pair, PgsString, Symbol, Syntax, Type, UnpackedHeapValue};
use super::parser::Parser;
use super::primitives;
use super::refs::{Fixnum, FrameTag, Primop, StatefulDisplay, UnpackedValue, Value};
use super::state::State;

#[derive(Debug)]
pub struct SyntaxError(Value);

impl StatefulDisplay for SyntaxError {
    fn st_fmt(&self, state: &State, f: &mut Formatter) -> fmt::Result {
        write!(f, "Bad syntax: {}", self.0.fmt_wrap(state))
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    NonObject(Value),
    Inflexible(Value),
    Type { expected: Type, value: Value },
    FixnumOverflow,
    FlonumOverflow,
    Bounds { value: Value, index: Fixnum, len: usize },
    Argc { callee: Value, params: (usize, bool), got: usize },
    Retc { cont_params: (usize, bool), got: usize },
    Uncallable(Value),
    Unbound(Symbol),
    NotInPath(PgsString),
    IO(io::Error),
    NonFrameTag(Value)
}

impl StatefulDisplay for RuntimeError {
    fn st_fmt(&self, state: &State, f: &mut Formatter) -> fmt::Result {
        match self {
            RuntimeError::NonObject(value) =>
                write!(f, "{} is not a heap object", value.fmt_wrap(state)),
            RuntimeError::Inflexible(value) =>
                write!(f, "{} does not have an indexed field", value.fmt_wrap(state)),
            RuntimeError::Type { expected, value } =>
                write!(f, "Type error: {} is not of type {}", value.fmt_wrap(state), expected),
            RuntimeError::FixnumOverflow => write!(f, "fixnum overflow"),
            RuntimeError::FlonumOverflow => write!(f, "flonum overflow"),
            RuntimeError::Bounds { value, index, len } => write!(
                f,
                "Out of bounds indexing {} of length {} with {}",
                value.fmt_wrap(state),
                len,
                index
            ),
            RuntimeError::Argc { callee, params: (paramc, variadic), got } => {
                write!(f, "{} expected ", callee.fmt_wrap(state))?;
                if *variadic {
                    write!(f, "at least")?;
                } else {
                    write!(f, "exactly")?;
                }
                write!(f, " {} arguments but was passed {}", paramc, got)
            },
            RuntimeError::Retc { cont_params: (paramc, variadic), got } => {
                write!(f, "expected ")?;
                if *variadic {
                    write!(f, "at least")?;
                } else {
                    write!(f, "exactly")?;
                }
                write!(f, " {} return values but got {}", paramc, got)
            },
            RuntimeError::Uncallable(v) => write!(f, "{} cannot be called", v.fmt_wrap(state)),
            RuntimeError::Unbound(name) => write!(f, "Unbound variable: {}", name),
            RuntimeError::NotInPath(filename) =>
                write!(f, "File {} not found on *include-path*", filename),
            RuntimeError::IO(io_err) => write!(f, "IO error: {}", io_err),
            RuntimeError::NonFrameTag(v) => write!(f, "Not a frame tag: {}", v.fmt_wrap(state))
        }
    }
}

pub enum Op {
    Eval,
    Continue,
    Apply,
    Stop
}

pub fn run(state: &mut State) -> Result<(), PgsError> {
    let mut op = Op::Eval;
    let expr = state.pop::<Value>().unwrap().unwrap();
    state.push_env();
    state.push(FrameTag::Done);
    state.push(expr);

    loop {
        op = match op {
            Op::Eval => eval(state)?,
            Op::Continue => continu(state)?,
            Op::Apply => apply(state)?,
            Op::Stop => return Ok(())
        };
    }
}

fn eval(state: &mut State) -> Result<Op, PgsError> {
    match state.peek().unwrap().unpack() {
        UnpackedValue::ORef(oref) => match oref.unpack(state) {
            UnpackedHeapValue::Pair(pair) => {
                if let Ok(callee) = state.downcast::<Syntax>(pair.car) {
                    if let Ok(sym) = state.downcast::<Symbol>(callee.datum) {
                        match sym.as_str() {
                            "define" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    // FIXME: Fail if not on toplevel
                                    if let Ok(syntax) = state.downcast::<Syntax>(args.car) {
                                        if let Ok(name) = state.downcast::<Symbol>(syntax.datum) {
                                            if let Ok(rargs) = state.downcast::<Pair>(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop::<Value>().unwrap().unwrap();
                                                    state.push(name);
                                                    state.push_env();
                                                    state.push(FrameTag::Define);
                                                    state.push(value_expr);
                                                    return Ok(Op::Eval);
                                                }
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "set!" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    if let Ok(syntax) = state.downcast::<Syntax>(args.car) {
                                        if let Ok(name) = state.downcast::<Symbol>(syntax.datum) {
                                            if let Ok(rargs) = state.downcast::<Pair>(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop::<Value>().unwrap().unwrap();
                                                    state.push(name);
                                                    state.push_env();
                                                    state.push(FrameTag::Set);
                                                    state.push(value_expr);
                                                    return Ok(Op::Eval);
                                                }
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "begin" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    let stmt = args.car;

                                    if args.cdr == Value::NIL {
                                        state.pop::<Value>().unwrap().unwrap();
                                        state.push(stmt);
                                        return Ok(Op::Eval);
                                    } else {
                                        state.pop::<Value>().unwrap().unwrap();
                                        state.push(args.cdr);
                                        state.push_env();
                                        state.push(FrameTag::Stmt);
                                        state.push(stmt);
                                        return Ok(Op::Eval);
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "if" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    let condition = args.car;

                                    if let Ok(branches) = state.downcast::<Pair>(args.cdr) {
                                        let succeed = branches.car;

                                        if let Ok(rargs) = state.downcast::<Pair>(branches.cdr) {
                                            let fail = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                state.pop::<Value>().unwrap().unwrap();
                                                state.push(succeed);
                                                state.push(fail);
                                                state.push_env();
                                                state.push(FrameTag::CondBranch);
                                                state.push(condition);
                                                return Ok(Op::Eval);
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "syntax" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        state.pop::<Value>().unwrap().unwrap();
                                        state.push(args.car);
                                        state.push(1u16);
                                        return Ok(Op::Continue);
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "quote" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        state.pop::<Value>().unwrap().unwrap();
                                        let datum = unsafe { args.car.to_datum(state) };
                                        state.push(datum);
                                        state.push(1u16);
                                        return Ok(Op::Continue);
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "let*" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    let bindings = args.car;

                                    if let Ok(rargs) = state.downcast::<Pair>(args.cdr) {
                                        let body = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            if let Ok(bindings) = state.downcast::<Syntax>(bindings)
                                            {
                                                if let Ok(bindings) =
                                                    state.downcast::<Pair>(bindings.datum)
                                                {
                                                    let binding = bindings.car;

                                                    if let Ok(binding) =
                                                        state.downcast::<Syntax>(binding)
                                                    {
                                                        if let Ok(binding) =
                                                            state.downcast::<Pair>(binding.datum)
                                                        {
                                                            let binder = binding.car;

                                                            if let Ok(exprs) =
                                                                state.downcast::<Pair>(binding.cdr)
                                                            {
                                                                let expr = exprs.car;

                                                                if exprs.cdr == Value::NIL {
                                                                    state
                                                                        .pop::<Value>()
                                                                        .unwrap()
                                                                        .unwrap();
                                                                    state.push(body);
                                                                    state.push(bindings.cdr);
                                                                    state.push(binder);
                                                                    state.push_env();
                                                                    state.push(FrameTag::Let);
                                                                    state.push(expr);
                                                                    return Ok(Op::Eval);
                                                                }
                                                            }
                                                        }
                                                    }
                                                } else if bindings.datum == Value::NIL {
                                                    state.pop::<Value>().unwrap().unwrap();
                                                    state.push(body);
                                                    return Ok(Op::Eval);
                                                }
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "lambda" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    let params = args.car;

                                    if let Ok(rargs) = state.downcast::<Pair>(args.cdr) {
                                        let body = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            state.pop::<Value>().unwrap().unwrap();
                                            state.push_env();
                                            state.push(body);

                                            if let Ok(params) = state.downcast::<Syntax>(params) {
                                                let mut params = params.datum;
                                                let mut arity = 0;

                                                while let Ok(param_pair) =
                                                    state.downcast::<Pair>(params)
                                                {
                                                    if let Ok(param) =
                                                        state.downcast::<Syntax>(param_pair.car)
                                                    {
                                                        if let Ok(param) =
                                                            state.downcast::<Symbol>(param.datum)
                                                        {
                                                            arity += 1;
                                                            params = param_pair.cdr;
                                                            state.push(param);
                                                        } else {
                                                            state.raise(SyntaxError(
                                                                param_pair.car
                                                            ))?;
                                                        }
                                                    }
                                                }

                                                if params == Value::NIL {
                                                    state.insert_after(arity, params);
                                                } else {
                                                    let params = if let Ok(params) =
                                                        state.downcast::<Syntax>(params)
                                                    {
                                                        params.datum
                                                    } else {
                                                        params
                                                    };

                                                    if let Ok(params) =
                                                        state.downcast::<Symbol>(params)
                                                    {
                                                        state.insert_after(arity, params.into());
                                                    } else {
                                                        state.raise(SyntaxError(params.into()))?;
                                                    }
                                                }

                                                unsafe {
                                                    state.vector(arity.try_into()?);
                                                    state.closure(Primop::Call, 4.into());
                                                }
                                                state.push(1u16);
                                                return Ok(Op::Continue);
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "include" => {
                                if let Ok(args) = state.downcast::<Pair>(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        if let Ok(filename) = state.downcast::<Syntax>(args.car) {
                                            if let Ok(filename) =
                                                state.downcast::<PgsString>(filename.datum)
                                            {
                                                if let Some(path) = state.resolve_path(&filename) {
                                                    match fs::read_to_string(&path) {
                                                        Ok(contents) => {
                                                            state.pop::<Value>().unwrap().unwrap();
                                                            let mut parser = Parser::new(
                                                                Lexer::new(contents.chars())
                                                            );

                                                            match unsafe {
                                                                parser.sexprs(
                                                                    state,
                                                                    path.to_str().unwrap()
                                                                )
                                                            } {
                                                                Ok(()) => {},
                                                                Err(err) => state.raise(err)?
                                                            }
                                                            return Ok(Op::Eval);
                                                        },
                                                        Err(io_err) =>
                                                            state.raise(RuntimeError::IO(io_err))?,
                                                    }
                                                } else {
                                                    state.raise(RuntimeError::NotInPath(
                                                        filename.into()
                                                    ))?
                                                }
                                            }
                                        }
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            _ => {}
                        }
                    }
                }

                state.pop::<Value>().unwrap().unwrap();
                state.push(pair.cdr);
                state.push(0u16);
                state.push_env();
                state.push(FrameTag::Arg);
                state.push(pair.car);
                Ok(Op::Eval)
            },
            UnpackedHeapValue::Symbol(_) => {
                state.lookup()?;
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::Vector(_) => {
                let vec = state.pop::<Value>().unwrap().unwrap();
                let vec = unsafe { vec.to_datum(state) };
                state.push(vec);
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::Syntax(syntax) => {
                state.pop::<Value>().unwrap().unwrap();
                state.push(syntax.datum);
                Ok(Op::Eval)
            },
            _ => {
                state.push(1u16);
                Ok(Op::Continue)
            }
        },
        UnpackedValue::Fixnum(_) => {
            state.push(1u16);
            Ok(Op::Continue)
        },
        UnpackedValue::Flonum(_) => {
            state.push(1u16);
            Ok(Op::Continue)
        },
        UnpackedValue::Char(_) => {
            state.push(1u16);
            Ok(Op::Continue)
        },
        UnpackedValue::Bool(_) => {
            state.push(1u16);
            Ok(Op::Continue)
        },
        UnpackedValue::Unspecified => {
            state.push(1u16);
            Ok(Op::Continue)
        },
        UnpackedValue::Nil => state.raise(SyntaxError(Value::NIL)),
        UnpackedValue::FrameTag(_) => unreachable!()
    }
}

fn continu(state: &mut State) -> Result<Op, PgsError> {
    let value_count: usize = state.pop().unwrap()?;
    state.set_env(state.downcast::<Bindings>(state.get(value_count + 1).unwrap()).unwrap());
    match unsafe { transmute::<Value, FrameTag>(state.get(value_count).unwrap()) } {
        FrameTag::Done => {
            if value_count == 1 {
                let res: Value = state.pop().unwrap().unwrap();
                state.pop::<Value>().unwrap().unwrap(); // frame tag
                state.pop::<Value>().unwrap().unwrap(); // env
                state.push(res);
                Ok(Op::Stop)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Define => {
            if value_count == 1 {
                let value: Value = state.pop().unwrap().unwrap();
                state.pop::<Value>().unwrap().unwrap(); // frame tag
                state.pop::<Value>().unwrap().unwrap(); // env
                state.push(value);
                unsafe { state.define()? };
                state.push(Value::UNSPECIFIED);
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Set => {
            if value_count == 1 {
                let value: Value = state.pop().unwrap().unwrap();
                state.pop::<Value>().unwrap().unwrap(); // frame tag
                state.pop::<Value>().unwrap().unwrap(); // env
                state.push(value);
                state.set()?;
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::CondBranch => {
            if value_count == 1 {
                if state.pop::<Value>().unwrap().unwrap() != Value::FALSE {
                    state.pop::<Value>().unwrap().unwrap(); // frame tag
                    state.pop::<Value>().unwrap().unwrap(); // env
                    state.pop::<Value>().unwrap().unwrap(); // #f branch
                } else {
                    state.pop::<Value>().unwrap().unwrap(); // frame tag
                    state.pop::<Value>().unwrap().unwrap(); // env
                    let branch: Value = state.pop().unwrap().unwrap();
                    state.pop::<Value>().unwrap().unwrap(); // non-#f branch
                    state.push(branch);
                }
                Ok(Op::Eval)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Let => {
            if value_count == 1 {
                if let Ok(name) = state.downcast::<Syntax>(state.get(3).unwrap()) {
                    if let Ok(name) = state.downcast::<Symbol>(name.datum) {
                        state.insert_after(1, name.into());
                        unsafe {
                            state.push_scope();
                            state.define()?;
                        }

                        let bindings = state.get(3).unwrap();

                        if let Ok(bindings) = state.downcast::<Pair>(bindings) {
                            let binding = bindings.car;

                            if let Ok(binding) = state.downcast::<Syntax>(binding) {
                                if let Ok(binding) = state.downcast::<Pair>(binding.datum) {
                                    let binder = binding.car;

                                    if let Ok(exprs) = state.downcast::<Pair>(binding.cdr) {
                                        let expr = exprs.car;

                                        if exprs.cdr == Value::NIL {
                                            state.put(3, bindings.cdr);
                                            state.put(2, binder);
                                            state.put(1, state.env().into());
                                            state.push(expr);
                                            return Ok(Op::Eval);
                                        }
                                    }
                                }
                            }

                            state.raise(SyntaxError(binding))?;
                        } else if bindings == Value::NIL {
                            state.pop::<Value>().unwrap().unwrap(); // frame tag
                            state.pop::<Value>().unwrap().unwrap(); // env
                            state.pop::<Value>().unwrap().unwrap(); // name
                            state.pop::<Value>().unwrap().unwrap(); // bindings
                            return Ok(Op::Eval);
                        } else {
                            state.raise(SyntaxError(bindings))?;
                        }
                    }

                    state.raise(SyntaxError(state.get(3).unwrap()))?;
                }

                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Stmt => {
            let stmts = state.get(value_count + 2).unwrap();

            if let Ok(stmts) = state.downcast::<Pair>(stmts) {
                for _ in 0..value_count {
                    state.pop::<Value>().unwrap().unwrap();
                }
                state.put(2, stmts.cdr);
                state.push(stmts.car);
                Ok(Op::Eval)
            } else if stmts == Value::NIL {
                for _ in 0..(FrameTag::Stmt.framesize().0 + 1) {
                    state.remove(value_count);
                }
                state.push(Value::try_from(value_count).unwrap());
                Ok(Op::Continue)
            } else {
                state.raise(SyntaxError(stmts))
            }
        },
        FrameTag::Arg => {
            if value_count == 1 {
                let value: Value = state.pop().unwrap().unwrap();
                let i = state.get(2).unwrap();
                let i: usize = state.downcast(i)?;
                let rargs = state.get(3 + i).unwrap();

                // OPTIMIZE:
                if let Ok(rargs) = state.downcast::<Pair>(rargs) {
                    state.pop::<Value>().unwrap().unwrap(); // frame tag
                    state.pop::<Value>().unwrap().unwrap(); // env
                    state.pop::<Value>().unwrap().unwrap(); // i
                    state.put(i, rargs.cdr);
                    state.push(value);
                    state.push(Value::try_from(i + 1).unwrap());
                    state.push_env();
                    state.push(FrameTag::Arg);
                    state.push(rargs.car);
                    Ok(Op::Eval)
                } else if rargs == Value::NIL {
                    state.pop::<Value>().unwrap().unwrap(); // frame tag
                    state.pop::<Value>().unwrap().unwrap(); // env
                    state.pop::<Value>().unwrap().unwrap(); // i
                    state.push(value);
                    state.push(Value::try_from(i).unwrap()); // argc for apply
                    state.remove(i + 2); // rargs
                    Ok(Op::Apply)
                } else {
                    state.raise(SyntaxError(rargs))
                }
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::CallWithValues => {
            state.remove(value_count).unwrap(); // frame tag
            state.remove(value_count).unwrap(); // env
            state.push(Value::try_from(value_count).unwrap());
            Ok(Op::Apply)
        }
    }
}

fn apply(state: &mut State) -> Result<Op, PgsError> {
    // ... callee arg{argc} argc
    let arg_count = state.peek().unwrap();
    let arg_count: usize = state.downcast(arg_count)?;

    if let Ok(callee) = state.downcast::<Closure>(state.get(arg_count + 1).unwrap()) {
        primitives::perform(callee.code, state)
    } else {
        state.raise(RuntimeError::Uncallable(state.get(arg_count + 1).unwrap()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_const() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        state.push(Value::from('a'));
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from('a'));

        unsafe { state.push_string("foo") };
        run(&mut state).unwrap();
        let res: PgsString = state.pop().unwrap().unwrap();

        assert_eq!(res.as_str(), "foo");
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        state.push(Value::NIL);
        let res = run(&mut state);

        assert!(res.is_err());
    }

    #[test]
    fn test_variables() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(define foo 5)".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("(set! foo 8)".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("foo".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_quote() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("'()".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        assert!(run(&mut state).is_err());
    }

    #[test]
    fn test_begin() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(begin 42 23)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from(23i16));

        let mut parser = Parser::new(Lexer::new("(begin)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        assert!(run(&mut state).is_err());
    }

    #[test]
    fn test_if() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(if #t 42 23)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from(42i16));
    }

    #[test]
    fn test_let() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser =
            Parser::new(Lexer::new("(let* ((a (if #f 5 8)) (b #f)) (if b 42 a))".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_lambda() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("((lambda (a b) b) 5 8)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop::<Value>().unwrap().unwrap(), Value::from(8i16));
    }
}

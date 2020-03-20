use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::io;
use std::mem::transmute;

use super::error::PgsError;
use super::lexer::Lexer;
use super::objects::{BuiltInType, Closure, Pair, PgsString, Symbol, Syntax, UnpackedHeapValue};
use super::parser::Parser;
use super::primitives;
use super::refs::{FrameTag, Primop, UnpackedValue, Value};
use super::state::State;

#[derive(Debug)]
pub struct SyntaxError(Value);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "Bad syntax: {}", self.0) }
}

#[derive(Debug)]
pub enum RuntimeError {
    Type { expected: BuiltInType, value: Value },
    Overflow(BuiltInType),
    Bounds { value: Value, index: isize, len: usize },
    Argc { callee: Value, params: (usize, bool), got: usize },
    Retc { cont_params: (usize, bool), got: usize },
    Uncallable(Value),
    Unbound(Symbol),
    NotInPath(Value), // HACK since PgsString seems unsized (but isn't really)
    IO(io::Error)
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RuntimeError::Type { expected, value } =>
                write!(f, "Type error: {} is not of type {}", value, expected),
            RuntimeError::Overflow(t) => write!(f, "{} overflow", t),
            RuntimeError::Bounds { value, index, len } =>
                write!(f, "Out of bounds indexing {} of length {} with {}", value, len, index),
            RuntimeError::Argc { callee, params: (paramc, variadic), got } => {
                write!(f, "{} expected ", callee)?;
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
            RuntimeError::Uncallable(v) => write!(f, "{} cannot be called", v),
            RuntimeError::Unbound(name) => write!(f, "Unbound variable: {}", name),
            RuntimeError::NotInPath(filename) =>
                write!(f, "File {} not found on *include-path*", filename),
            RuntimeError::IO(io_err) => write!(f, "IO error: {}", io_err)
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
    let expr = state.pop().unwrap();
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
        UnpackedValue::ORef(oref) => match oref.unpack() {
            UnpackedHeapValue::Pair(pair) => {
                if let Ok(callee) = Syntax::try_from(pair.car) {
                    if let Ok(sym) = Symbol::try_from(callee.datum) {
                        match sym.as_str() {
                            "define" => {
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    // FIXME: Fail if not on toplevel
                                    if let Ok(syntax) = Syntax::try_from(args.car) {
                                        if let Ok(name) = Symbol::try_from(syntax.datum) {
                                            if let Ok(rargs) = Pair::try_from(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop();
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    if let Ok(syntax) = Syntax::try_from(args.car) {
                                        if let Ok(name) = Symbol::try_from(syntax.datum) {
                                            if let Ok(rargs) = Pair::try_from(args.cdr) {
                                                let value_expr = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    state.pop();
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    let stmt = args.car;

                                    if args.cdr == Value::NIL {
                                        state.pop();
                                        state.push(stmt);
                                        return Ok(Op::Eval);
                                    } else {
                                        state.pop();
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    let condition = args.car;

                                    if let Ok(branches) = Pair::try_from(args.cdr) {
                                        let succeed = branches.car;

                                        if let Ok(rargs) = Pair::try_from(branches.cdr) {
                                            let fail = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                state.pop();
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        state.pop();
                                        state.push(args.car);
                                        state.push(1u16);
                                        return Ok(Op::Continue);
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "quote" => {
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        state.pop();
                                        let datum = unsafe { args.car.to_datum(state) };
                                        state.push(datum);
                                        state.push(1u16);
                                        return Ok(Op::Continue);
                                    }
                                }

                                state.raise(SyntaxError(pair.into()))?;
                            },
                            "let*" => {
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    let bindings = args.car;

                                    if let Ok(rargs) = Pair::try_from(args.cdr) {
                                        let body = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            if let Ok(bindings) = Syntax::try_from(bindings) {
                                                if let Ok(bindings) = Pair::try_from(bindings.datum)
                                                {
                                                    let binding = bindings.car;

                                                    if let Ok(binding) = Syntax::try_from(binding) {
                                                        if let Ok(binding) =
                                                            Pair::try_from(binding.datum)
                                                        {
                                                            let binder = binding.car;

                                                            if let Ok(exprs) =
                                                                Pair::try_from(binding.cdr)
                                                            {
                                                                let expr = exprs.car;

                                                                if exprs.cdr == Value::NIL {
                                                                    state.pop();
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
                                                    state.pop();
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    let params = args.car;

                                    if let Ok(rargs) = Pair::try_from(args.cdr) {
                                        let body = rargs.car;

                                        if rargs.cdr == Value::NIL {
                                            state.pop();
                                            state.push_env();
                                            state.push(body);

                                            if let Ok(params) = Syntax::try_from(params) {
                                                let mut params = params.datum;
                                                let mut arity = 0;

                                                while let Ok(param_pair) = Pair::try_from(params) {
                                                    if let Ok(param) =
                                                        Syntax::try_from(param_pair.car)
                                                    {
                                                        if let Ok(param) =
                                                            Symbol::try_from(param.datum)
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
                                                        Syntax::try_from(params)
                                                    {
                                                        params.datum
                                                    } else {
                                                        params
                                                    };

                                                    if let Ok(params) = Symbol::try_from(params) {
                                                        state.insert_after(arity, params.into());
                                                    } else {
                                                        state.raise(SyntaxError(params.into()))?;
                                                    }
                                                }

                                                unsafe {
                                                    state.vector(arity);
                                                    state.closure(Primop::Call, 4);
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
                                if let Ok(args) = Pair::try_from(pair.cdr) {
                                    if args.cdr == Value::NIL {
                                        if let Ok(filename) = Syntax::try_from(args.car) {
                                            if let Ok(filename) =
                                                PgsString::try_from(filename.datum)
                                            {
                                                if let Some(path) = state.resolve_path(&filename) {
                                                    match fs::read_to_string(&path) {
                                                        Ok(contents) => {
                                                            state.pop();
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

                state.pop();
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
                let vec = state.pop().unwrap();
                let vec = unsafe { vec.to_datum(state) };
                state.push(vec);
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::String(_) => {
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::Closure(_) => {
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::Bindings(_) => {
                state.push(1u16);
                Ok(Op::Continue)
            },
            UnpackedHeapValue::Syntax(syntax) => {
                state.pop().unwrap();
                state.push(syntax.datum);
                Ok(Op::Eval)
            },
            UnpackedHeapValue::Record(_) => {
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
        UnpackedValue::Unbound => unreachable!()
    }
}

fn continu(state: &mut State) -> Result<Op, PgsError> {
    let value_count: usize = state.pop().unwrap().try_into().unwrap();
    state.set_env(state.get(value_count + 1).unwrap().try_into().unwrap());
    match unsafe { transmute::<Value, FrameTag>(state.get(value_count).unwrap()) } {
        FrameTag::Done => {
            if value_count == 1 {
                let res = state.pop().unwrap();
                state.pop(); // frame tag
                state.pop(); // env
                state.push(res);
                Ok(Op::Stop)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Define => {
            if value_count == 1 {
                let value = state.pop().unwrap();
                state.pop(); // frame tag
                state.pop(); // env
                state.push(value);
                unsafe { state.define() };
                state.push(Value::UNSPECIFIED);
                state.push(1u16);
                Ok(Op::Continue)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Set => {
            if value_count == 1 {
                let value = state.pop().unwrap();
                state.pop(); // frame tag
                state.pop(); // env
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
                if state.pop().unwrap() != Value::FALSE {
                    state.pop(); // frame tag
                    state.pop(); // env
                    state.pop(); // #f branch
                } else {
                    state.pop(); // frame tag
                    state.pop(); // env
                    let branch = state.pop().unwrap();
                    state.pop(); // non-#f branch
                    state.push(branch);
                }
                Ok(Op::Eval)
            } else {
                state.raise(RuntimeError::Retc { cont_params: (1, false), got: value_count })
            }
        },
        FrameTag::Let => {
            if value_count == 1 {
                if let Ok(name) = Syntax::try_from(state.get(3).unwrap()) {
                    if let Ok(name) = Symbol::try_from(name.datum) {
                        state.insert_after(1, name.into());
                        unsafe {
                            state.push_scope();
                            state.define();
                        }

                        let bindings = state.get(3).unwrap();

                        if let Ok(bindings) = Pair::try_from(bindings) {
                            let binding = bindings.car;

                            if let Ok(binding) = Syntax::try_from(binding) {
                                if let Ok(binding) = Pair::try_from(binding.datum) {
                                    let binder = binding.car;

                                    if let Ok(exprs) = Pair::try_from(binding.cdr) {
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
                            state.pop(); // frame tag
                            state.pop(); // env
                            state.pop(); // name
                            state.pop(); // bindings
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

            if let Ok(stmts) = Pair::try_from(stmts) {
                for _ in 0..value_count {
                    state.pop();
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
                let value = state.pop().unwrap();
                let i: usize = state.get(2).unwrap().try_into().unwrap();
                let rargs = state.get(3 + i).unwrap();

                // OPTIMIZE:
                if let Ok(rargs) = Pair::try_from(rargs) {
                    state.pop(); // frame tag
                    state.pop(); // env
                    state.pop(); // i
                    state.put(i, rargs.cdr);
                    state.push(value);
                    state.push(Value::try_from(i + 1).unwrap());
                    state.push_env();
                    state.push(FrameTag::Arg);
                    state.push(rargs.car);
                    Ok(Op::Eval)
                } else if rargs == Value::NIL {
                    state.pop(); // frame tag
                    state.pop(); // env
                    state.pop(); // i
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
    let arg_count: usize = state.peek().unwrap().try_into().unwrap();

    if let Ok(callee) = Closure::try_from(state.get(arg_count + 1).unwrap()) {
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
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        state.push(Value::from('a'));
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from('a'));

        unsafe { state.push_string("foo") };
        run(&mut state).unwrap();
        let res: PgsString = state.pop().unwrap().try_into().unwrap();

        assert_eq!(res.as_str(), "foo");
    }

    #[test]
    fn test_nil() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        state.push(Value::NIL);
        let res = run(&mut state);

        assert!(res.is_err());
    }

    #[test]
    fn test_variables() {
        let mut state = State::new(&[], 1 << 14, 1 << 20); // HACK: bigger heap since it grow not

        let mut parser = Parser::new(Lexer::new("(define foo 5)".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("(set! foo 8)".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("foo".chars()));
        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();
        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_quote() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        let mut parser = Parser::new(Lexer::new("'()".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        assert!(run(&mut state).is_err());
    }

    #[test]
    fn test_begin() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(begin 42 23)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(23i16));

        let mut parser = Parser::new(Lexer::new("(begin)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        assert!(run(&mut state).is_err());
    }

    #[test]
    fn test_if() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(if #t 42 23)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(42i16));
    }

    #[test]
    fn test_let() {
        let mut state = State::new(&[], 1 << 14, 1 << 20); // HACK: 14 because heap is ungrowing ATM.

        let mut parser =
            Parser::new(Lexer::new("(let* ((a (if #f 5 8)) (b #f)) (if b 42 a))".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_lambda() {
        let mut state = State::new(&[], 1 << 14, 1 << 20);

        let mut parser = Parser::new(Lexer::new("((lambda (a b) b) 5 8)".chars()));

        unsafe { parser.sexprs(&mut state, "test").unwrap() };
        run(&mut state).unwrap();

        assert_eq!(state.pop().unwrap(), Value::from(8i16));
    }
}

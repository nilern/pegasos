use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::io;
use std::path::PathBuf;

use super::error::PgsError;
use super::lexer::Lexer;
use super::objects::{Bindings, Closure, Pair, PgsString, Symbol, Syntax, Type, UnpackedHeapValue, Vector};
use super::parser::Parser;
use super::primitives;
use super::refs::{DynamicDowncast, Fixnum, Primop, UnpackedValue, Value};
use super::state::{self, with_retry, State};

// ---

pub struct Interpreter;

impl Interpreter {
    pub fn new(path: &[PathBuf], initial_heap: usize, max_heap: usize) -> Self {
        state::initialize(path, initial_heap, max_heap);
        Self
    }

    pub fn run(&mut self) -> Result<(), PgsError> {
        let expr = state::with_mut(State::pop).unwrap().unwrap();
        let env = state::with(|state| state.env);
        let res = run(expr, env)?;
        state::with_mut(|state| state.push(res));
        Ok(())
    }
}

// ---

#[derive(Debug)]
pub struct SyntaxError(Value);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "Bad syntax: {}", self.0) }
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

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RuntimeError::NonObject(value) => write!(f, "{} is not a heap object", value),
            RuntimeError::Inflexible(value) => write!(f, "{} does not have an indexed field", value),
            RuntimeError::Type { expected, value } => write!(f, "Type error: {} is not of type {}", value, expected),
            RuntimeError::FixnumOverflow => write!(f, "fixnum overflow"),
            RuntimeError::FlonumOverflow => write!(f, "flonum overflow"),
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
            RuntimeError::NotInPath(filename) => write!(f, "File {} not found on *include-path*", filename),
            RuntimeError::IO(io_err) => write!(f, "IO error: {}", io_err),
            RuntimeError::NonFrameTag(v) => write!(f, "Not a frame tag: {}", v)
        }
    }
}

pub enum Op {
    Eval,
    Continue,
    Apply,
    Stop
}

// TODO: Remove if no-op
fn run(expr: Value, env: Bindings) -> Result<Value, PgsError> { eval(expr, env) }

fn eval(mut expr: Value, mut env: Bindings) -> Result<Value, PgsError> {
    loop {
        match expr.unpack() {
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
                                                    if rargs.cdr == Value::NIL {
                                                        let value = eval(rargs.car, env)?;
                                                        with_retry(|| env.insert(name, value).ok());
                                                        return Ok(Value::UNSPECIFIED);
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "set!" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if let Ok(syntax) = Syntax::try_from(args.car) {
                                            if let Ok(name) = Symbol::try_from(syntax.datum) {
                                                if let Ok(rargs) = Pair::try_from(args.cdr) {
                                                    if rargs.cdr == Value::NIL {
                                                        let value = eval(rargs.car, env)?;
                                                        env.set(name, value).or(Err(RuntimeError::Unbound(name)));
                                                        return Ok(Value::UNSPECIFIED);
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "begin" => {
                                    if let Ok(mut stmts) = Pair::try_from(pair.cdr) {
                                        loop {
                                            if let Ok(stmts_) = Pair::try_from(stmts.cdr) {
                                                eval(stmts.car, env)?;
                                                stmts = stmts_;
                                            } else if stmts.cdr == Value::NIL {
                                                expr = stmts.car;
                                                continue;
                                            } else {
                                                return Err(SyntaxError(pair.into()).into());
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "if" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let condition = args.car;

                                        if let Ok(branches) = Pair::try_from(args.cdr) {
                                            let succeed = branches.car;

                                            if let Ok(rargs) = Pair::try_from(branches.cdr) {
                                                let fail = rargs.car;

                                                if rargs.cdr == Value::NIL {
                                                    if eval(condition, env)? != Value::FALSE {
                                                        expr = succeed;
                                                        continue;
                                                    } else {
                                                        expr = fail;
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "syntax" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if args.cdr == Value::NIL {
                                            return Ok(args.car);
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "quote" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if args.cdr == Value::NIL {
                                            return Ok(args.car.to_datum());
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "let*" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let bindings = args.car;

                                        if let Ok(rargs) = Pair::try_from(args.cdr) {
                                            let body = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                if let Ok(bindings) = Syntax::try_from(bindings) {
                                                    let mut bindings = bindings.datum;

                                                    loop {
                                                        if let Ok(bindings_pair) = Pair::try_from(bindings) {
                                                            let binding = bindings_pair.car;

                                                            if let Ok(binding) = Syntax::try_from(binding) {
                                                                if let Ok(binding) = Pair::try_from(binding.datum) {
                                                                    let binder = binding.car;

                                                                    if let Ok(name) = Symbol::try_from(binder) {
                                                                        if let Ok(exprs) = Pair::try_from(binding.cdr) {
                                                                            let expr = exprs.car;

                                                                            if exprs.cdr == Value::NIL {
                                                                                let value = eval(expr, env)?;
                                                                                bindings = bindings_pair.cdr;
                                                                                env = Bindings::new(Some(env));
                                                                                with_retry(|| {
                                                                                    env.insert(name, value).ok()
                                                                                });
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        } else if bindings.datum == Value::NIL {
                                                            expr = body;
                                                            continue;
                                                        } else {
                                                            return Err(SyntaxError(pair.into()).into());
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "lambda" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        let params = args.car;

                                        if let Ok(rargs) = Pair::try_from(args.cdr) {
                                            let body = rargs.car;

                                            if rargs.cdr == Value::NIL {
                                                if let Ok(params) = Syntax::try_from(params) {
                                                    let mut params = params.datum;
                                                    let mut fixeds = Vec::new();
                                                    let mut arity = 0;

                                                    while let Ok(param_pair) = Pair::try_from(params) {
                                                        if let Ok(param) = Syntax::try_from(param_pair.car) {
                                                            if let Ok(param) = Symbol::try_from(param.datum) {
                                                                arity += 1;
                                                                params = param_pair.cdr;
                                                                fixeds.push(param);
                                                            } else {
                                                                return Err(SyntaxError(param_pair.car).into());
                                                            }
                                                        }
                                                    }
                                                    let fixeds = Vector::from_slice(&fixeds);

                                                    let flex = if params == Value::NIL {
                                                        params
                                                    } else {
                                                        let params = if let Ok(params) = Syntax::try_from(params) {
                                                            params.datum
                                                        } else {
                                                            params
                                                        };

                                                        if let Ok(params) = Symbol::try_from(params) {
                                                            params.into()
                                                        } else {
                                                            return Err(SyntaxError(params.into()).into());
                                                        }
                                                    };

                                                    let closure = Closure::new(Primop::Call, 4.into());
                                                    closure.clovers_mut().copy_from_slice(&[
                                                        env.into(),
                                                        body,
                                                        flex,
                                                        fixeds.into()
                                                    ]);
                                                    return Ok(closure);
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                "include" => {
                                    if let Ok(args) = Pair::try_from(pair.cdr) {
                                        if args.cdr == Value::NIL {
                                            if let Ok(filename) = Syntax::try_from(args.car) {
                                                if let Ok(filename) = PgsString::try_from(filename.datum) {
                                                    if let Some(path) = state.resolve_path(&filename) {
                                                        match fs::read_to_string(&path) {
                                                            Ok(contents) => {
                                                                let mut parser =
                                                                    Parser::new(Lexer::new(contents.chars()));

                                                                match unsafe { parser.sexprs(path.to_str().unwrap()) } {
                                                                    Ok(stx) => {
                                                                        expr = stx;
                                                                        continue;
                                                                    },
                                                                    Err(err) => return Err(err.into())
                                                                }
                                                            },
                                                            Err(io_err) => return Err(RuntimeError::IO(io_err).into())
                                                        }
                                                    } else {
                                                        return Err(RuntimeError::NotInPath(filename.into()).into());
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    return Err(SyntaxError(pair.into()).into());
                                },
                                _ => {}
                            }
                        }
                    }

                    let callee = eval(pair.car, env)?;
                    let mut args_list = pair.cdr;
                    let mut args = Vec::new();

                    loop {
                        if let Ok(args_pair) = Pair::try_from(args_list) {
                            args.push(eval(args_pair.car, env)?);
                            args_list = args_pair.cdr;
                        } else if args_list == Value::NIL {
                            break;
                        } else {
                            return Err(SyntaxError(pair.into()).into());
                        }
                    }

                    let arg_count: usize = args.len();
                    if let Ok(callee) = Closure::try_from(callee) {
                        if let Primop::Call = callee.code {
                            match callee.clovers() {
                                &[closure_env, body, rest_param, params] => {
                                    let params: Vector = params.try_into().unwrap();
                                    let fixed_argc = params.len();
                                    let variadic = rest_param != Value::NIL;

                                    // OPTIMIZE?
                                    if arg_count == fixed_argc || variadic && arg_count >= fixed_argc {
                                        // FIXME: It is an error for a <variable> to appear more than
                                        // once
                                        env = Bindings::new(Some(Bindings::unchecked_downcast(closure_env)));

                                        for i in 0..fixed_argc {
                                            with_retry(|| env.insert(params[i], args[i]).ok());
                                        }

                                        if variadic {
                                            let mut rest_arg = Value::NIL;
                                            for i in (fixed_argc..arg_count).rev() {
                                                rest_arg = Pair::cons(args[i], rest_arg).into();
                                            }
                                            with_retry(|| env.insert(rest_param, rest_arg).ok());
                                        }

                                        expr = body;
                                        continue;
                                    } else {
                                        return Err(RuntimeError::Argc {
                                            callee: callee.into(),
                                            params: (fixed_argc, variadic),
                                            got: arg_count
                                        }
                                        .into());
                                    }
                                },
                                _ => unreachable!()
                            }
                        } else {
                            return primitives::perform(callee.code, state);
                        }
                    } else {
                        return Err(RuntimeError::Uncallable(callee).into());
                    }
                },
                UnpackedHeapValue::Symbol(name) => return env.get(name).ok_or(RuntimeError::Unbound(name).into()),
                UnpackedHeapValue::Vector(_) => return Ok(expr.to_datum()),
                UnpackedHeapValue::Syntax(syntax) => {
                    expr = syntax.datum;
                    continue;
                },
                _ => return Ok(expr)
            },
            UnpackedValue::Nil => return Err(SyntaxError(Value::NIL).into()),
            UnpackedValue::FrameTag(_) => unreachable!(),
            _ => return Ok(expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_const() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        state::with_mut(|state| state.push(Value::from('a')));
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from('a'));

        let s = unsafe { PgsString::new("foo") };
        state::with_mut(|state| state.push(s));
        interpreter.run().unwrap();
        let res: PgsString = state::with_mut(State::pop).unwrap().unwrap();

        assert_eq!(res.as_str(), "foo");
    }

    #[test]
    fn test_nil() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        state::with_mut(|state| state.push(Value::NIL));
        let res = interpreter.run();

        assert!(res.is_err());
    }

    #[test]
    fn test_variables() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(define foo 5)".chars()));
        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();
        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("(set! foo 8)".chars()));
        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();
        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::UNSPECIFIED);

        let mut parser = Parser::new(Lexer::new("foo".chars()));
        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();
        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_quote() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("'()".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::NIL);

        let mut parser = Parser::new(Lexer::new("(quote () ())".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        assert!(interpreter.run().is_err());
    }

    #[test]
    fn test_begin() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(begin 42 23)".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from(23i16));

        let mut parser = Parser::new(Lexer::new("(begin)".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        assert!(interpreter.run().is_err());
    }

    #[test]
    fn test_if() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(if #t 42 23)".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from(42i16));
    }

    #[test]
    fn test_let() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("(let* ((a (if #f 5 8)) (b #f)) (if b 42 a))".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from(8i16));
    }

    #[test]
    fn test_lambda() {
        let mut interpreter = Interpreter::new(&[], 1 << 20, 1 << 20);

        let mut parser = Parser::new(Lexer::new("((lambda (a b) b) 5 8)".chars()));

        unsafe { parser.sexprs("test").unwrap() };
        interpreter.run().unwrap();

        assert_eq!(state::with_mut(State::pop::<Value>).unwrap().unwrap(), Value::from(8i16));
    }
}

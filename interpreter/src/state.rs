use std::convert::TryInto;
use std::env;
use std::io;
use std::iter;
use std::mem::{size_of, transmute};
use std::path::{Path, PathBuf};

use strum::IntoEnumIterator;

use super::error::PgsError;
use super::gc::MemoryManager;
use super::interpreter::RuntimeError;
use super::objects::{
    Bindings, BindingsData, Closure, ClosureData, FieldDescriptor, FieldDescriptorData, Header,
    Object, Pair, PairData, PgsString, PgsStringData, Symbol, SymbolData, SymbolTable, Syntax,
    SyntaxObject, Type, TypeData, Vector, VectorData
};
use super::parser::Loc;
use super::refs::{
    DynamicDowncast, DynamicType, Fixnum, FrameTag, HeapValue, Primop, StatefulDisplay, Tag, Value
};
use super::util::{Bool, False, True};

// ---

pub struct State {
    immediate_types: [Value; 8],
    object_types: BuiltinObjectTypes,
    heap: MemoryManager<Object>,
    symbol_table: SymbolTable,
    stack: Vec<Value>,
    env: Bindings
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct BuiltinObjectTypesStruct {
    pub string: Type,
    pub symbol: Type,
    pub pair: Type,
    pub vector: Type,
    pub procedure: Type,
    pub environment: Type,
    pub syntax: Type,
    pub typ: Type,
    pub field_descriptor: Type
}

#[repr(C)]
union BuiltinObjectTypes {
    named: BuiltinObjectTypesStruct,
    indexed: [Value; 9]
}

impl State {
    const STACK_SIZE: usize = 1 << 12; // 4 kiB
    const STACK_LEN: usize = Self::STACK_SIZE / size_of::<Value>();

    // FIXME:
    pub fn new(path: &[PathBuf], initial_heap: usize, max_heap: usize) -> Self {
        unsafe fn new_pretype<T: DynamicType>(state: &mut State, type_t: Type) -> Type {
            let is_bytes = T::IsBytes::reify();
            let is_flex = T::IsFlex::reify();
            let field_count = if is_bytes {
                // FIXME: figure out bytes objects with fixed fields (e.g. Symbol)
                (size_of::<T>() != 0) as usize + is_flex as usize // HACK
            } else {
                size_of::<T>() / size_of::<Value>() + is_flex as usize
            };

            let mut t = Type::unchecked_downcast(
                state
                    .heap
                    .alloc(
                        Object::new(type_t),
                        size_of::<TypeData>()
                            + size_of::<Fixnum>()
                            + field_count as usize * size_of::<Value>()
                    )
                    .unwrap()
            );

            *t = TypeData {
                is_bytes: is_bytes.into(),
                is_flex: is_flex.into(),
                name: Symbol::unchecked_downcast(Value::UNBOUND), // HACK
                parent: Value::FALSE,
                min_size: (size_of::<T>() + is_flex as usize * size_of::<Fixnum>())
                    .try_into()
                    .unwrap()
            };
            *((&mut *t as *mut TypeData).add(1) as *mut Fixnum) = field_count.try_into().unwrap();

            t
        }

        fn new_builtin_type<T: DynamicType>(
            state: &mut State, name: &str, fields: &[(bool, Fixnum, &str)]
        ) -> Type {
            assert!(
                fields.len() == size_of::<T>() / size_of::<Value>() + T::IsFlex::reify() as usize
            );

            let name = Symbol::new(state, &format!("<{}>", name)).unwrap();
            let is_flex = T::IsFlex::reify();
            let fields: Vec<FieldDescriptor> = fields
                .iter()
                .map(|(mutable, size, name)| {
                    let name = Symbol::new(state, name).unwrap();
                    FieldDescriptor::new(state, *mutable, *size, name).unwrap()
                })
                .collect();
            Type::new(
                state,
                T::IsBytes::reify(),
                is_flex,
                name,
                None,
                (size_of::<T>() + is_flex as usize * size_of::<Fixnum>()).try_into().unwrap(),
                &fields
            )
            .unwrap()
        }

        let mut res = Self {
            heap: MemoryManager::new(initial_heap, max_heap),
            symbol_table: SymbolTable::new(),
            stack: Vec::with_capacity(Self::STACK_LEN),
            env: unsafe { transmute(Value::UNBOUND) }, // HACK
            immediate_types: [Value::FALSE; 8],        // So that the ORef one becomes #f
            object_types: BuiltinObjectTypes { indexed: [Value::UNBOUND; 9] }
        };

        unsafe {
            let mut typ = Type::unchecked_downcast(
                res.heap
                    .alloc(
                        Object::new(Type::unchecked_downcast(Value::UNBOUND)), // HACK
                        2 * (size_of::<TypeData>() + size_of::<Value>())
                    )
                    .unwrap()
            );
            *typ.header_mut() = Object::new(typ); // Close the circle and a new world is born
            *typ = TypeData {
                is_bytes: false.into(),
                is_flex: true.into(),
                name: Symbol::unchecked_downcast(Value::UNBOUND), // HACK
                parent: Value::FALSE,
                min_size: (size_of::<TypeData>() + size_of::<Value>()).try_into().unwrap()
            };
            *((&mut *typ as *mut TypeData).add(1) as *mut Fixnum) =
                ((size_of::<TypeData>() + size_of::<Fixnum>()) / size_of::<Value>())
                    .try_into()
                    .unwrap();
            res.object_types.named.typ = typ;

            let field_descriptor = new_pretype::<FieldDescriptorData>(&mut res, typ);
            res.object_types.named.field_descriptor = field_descriptor;
            let symbol = new_pretype::<SymbolData>(&mut res, typ);
            res.object_types.named.symbol = symbol;

            let value_size: Fixnum = size_of::<Value>().try_into().unwrap();
            let byte_size: Fixnum = size_of::<u8>().try_into().unwrap();

            let string = new_builtin_type::<PgsStringData>(&mut res, "string", &[(
                true, byte_size, "bytes"
            )]);
            let pair = new_builtin_type::<PairData>(&mut res, "pair", &[
                (true, value_size, "car"),
                (true, value_size, "cdr")
            ]);
            let vector = new_builtin_type::<VectorData>(&mut res, "vector", &[(
                true, value_size, "elements"
            )]);
            let procedure = new_builtin_type::<ClosureData>(&mut res, "procedure", &[
                (false, value_size, "code"),
                (false, value_size, "clovers")
            ]);
            let syntax = new_builtin_type::<SyntaxObject>(&mut res, "syntax", &[
                (false, value_size, "datum"),
                (false, value_size, "scopes"),
                (false, value_size, "source"),
                (false, value_size, "line"),
                (false, value_size, "column")
            ]);
            let environment = new_builtin_type::<BindingsData>(&mut res, "environment", &[
                (false, value_size, "parent"),
                (true, value_size, "keys"),
                (true, value_size, "values"),
                (true, value_size, "occupancy")
            ]);

            res.object_types.named = BuiltinObjectTypesStruct {
                string,
                symbol,
                pair,
                vector,
                procedure,
                environment,
                syntax,
                typ,
                field_descriptor
            };

            res.object_types.named.typ.name = Symbol::new(&mut res, "<type>").unwrap();
            let is_bytes_symbol = Symbol::new(&mut res, "bytes?").unwrap();
            let is_flex_symbol = Symbol::new(&mut res, "flex?").unwrap();
            let name_symbol = Symbol::new(&mut res, "name").unwrap();
            let parent_symbol = Symbol::new(&mut res, "parent").unwrap();
            let min_size_symbol = Symbol::new(&mut res, "min-size").unwrap();
            let fields_symbol = Symbol::new(&mut res, "fields").unwrap();
            let type_fields = [
                FieldDescriptor::new(&mut res, false, value_size, is_bytes_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, is_flex_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, name_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, parent_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, min_size_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, fields_symbol).unwrap()
            ];
            res.object_types.named.typ.fields_mut().copy_from_slice(&type_fields);

            res.object_types.named.field_descriptor.name =
                Symbol::new(&mut res, "<field-descriptor>").unwrap();
            let mutable_symbol = Symbol::new(&mut res, "mutable").unwrap();
            let size_symbol = Symbol::new(&mut res, "size").unwrap();
            let field_descriptor_fields = [
                FieldDescriptor::new(&mut res, false, value_size, mutable_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, size_symbol).unwrap(),
                FieldDescriptor::new(&mut res, false, value_size, name_symbol).unwrap()
            ];
            res.object_types
                .named
                .field_descriptor
                .fields_mut()
                .copy_from_slice(&field_descriptor_fields);

            res.object_types.named.symbol.name = Symbol::new(&mut res, "<symbol>").unwrap();
            let hash_symbol = Symbol::new(&mut res, "hash").unwrap();
            let symbol_fields = [
                FieldDescriptor::new(
                    &mut res,
                    false,
                    (size_of::<u64>() as u16).into(),
                    hash_symbol
                )
                .unwrap(),
                FieldDescriptor::new(&mut res, false, byte_size.into(), name_symbol).unwrap()
            ];
            res.object_types.named.symbol.fields_mut().copy_from_slice(&symbol_fields);

            res.env = Bindings::new(&mut res, None).unwrap();

            for &typ in res.object_types.indexed.clone().iter() {
                res.env.insert(&mut res, Type::unchecked_downcast(typ).name, typ).unwrap();
            }

            for tag in Tag::iter() {
                if tag != Tag::ORef {
                    let name = Symbol::new(&mut res, &format!("{}", tag)).unwrap();
                    // TODO: Make these not total lies:
                    let typ = Type::new(&mut res, true, false, name, None, 0.into(), &[]).unwrap();
                    (*typ.header_mut()).header = Header::from_hash(tag as usize);
                    res.immediate_types[tag as usize] = typ.into();
                    res.env.insert(&mut res, name, typ.into()).unwrap();
                }
            }

            res.push_symbol("*include-path*");
            for dir in path {
                res.push_string(dir.to_str().unwrap());
            }
            res.push(Value::NIL);
            for _ in path {
                res.cons();
            }
            res.define().unwrap();

            for op in Primop::iter() {
                res.push_symbol(&format!("{}", op));
                res.push_primitive(op);
                res.define().unwrap();
            }
        }

        res
    }

    pub fn immediate_types(&self) -> &[Value; 8] { &self.immediate_types }

    pub fn types(&self) -> &BuiltinObjectTypesStruct { unsafe { &self.object_types.named } }

    pub fn type_of(&self, v: Value) -> Type {
        let immediate_type = self.immediate_type(v);
        if immediate_type != Value::FALSE {
            unsafe { Type::unchecked_downcast(immediate_type) }
        } else {
            unsafe { HeapValue::<()>::unchecked_downcast(v).typ() }
        }
    }

    pub fn downcast<T: DynamicDowncast>(&self, v: Value) -> Result<T, RuntimeError> {
        T::downcast(self, v)
    }

    pub fn push<T: Into<Value>>(&mut self, v: T) {
        if self.stack.len() < self.stack.capacity() {
            self.stack.push(v.into());
        } else {
            unimplemented!() // FIXME: Handle stack overflow
        }
    }

    pub fn pop<T: DynamicDowncast>(&mut self) -> Option<Result<T, RuntimeError>> {
        // FIXME: handle stack underflow
        self.stack.pop().map(|v| self.downcast::<T>(v))
    }

    pub fn peek(&self) -> Option<Value> { self.stack.last().map(|&v| v) }

    pub fn swap(&mut self) {
        let len = self.stack.len();
        self.stack.swap(len - 2, len - 1);
    }

    pub fn dup(&mut self) { self.push(self.peek().unwrap()) }

    pub fn get(&self, i: usize) -> Option<Value> {
        self.stack.len().checked_sub(1 + i).map(|i| self.stack[i])
    }

    pub fn put(&mut self, i: usize, v: Value) {
        let i = self.stack.len().checked_sub(1 + i).unwrap();
        self.stack[i] = v;
    }

    pub fn insert_after(&mut self, i: usize, v: Value) {
        let i = self.stack.len().checked_sub(i).unwrap();
        self.stack.insert(i, v)
    }

    pub fn remove(&mut self, i: usize) -> Option<Value> {
        self.stack.len().checked_sub(1 + i).map(|i| self.stack.remove(i))
    }

    pub fn immediate_type(&self, v: Value) -> Value { self.immediate_types[v.tag() as usize] }

    pub fn alloc<T: DynamicType<IsFlex = False>>(&mut self) -> Option<HeapValue<T>> {
        self.heap
            .alloc(Object::new(T::reify(self)), size_of::<T>())
            .map(|v| unsafe { transmute::<Value, HeapValue<T>>(v) })
    }

    pub fn alloc_flex<T: DynamicType<IsFlex = True>>(
        &mut self, flex_count: Fixnum
    ) -> Option<HeapValue<T>> {
        self.heap
            .alloc(
                Object::new(T::reify(self)),
                size_of::<T>()
                    + size_of::<Fixnum>()
                    + if T::IsBytes::reify() { size_of::<u8>() } else { size_of::<Value>() }
                        * <Fixnum as Into<usize>>::into(flex_count)
            )
            .map(|v| unsafe {
                let v = HeapValue::<T>::unchecked_downcast(v);
                *(v.data().add(size_of::<T>()) as *mut Fixnum) = flex_count;
                v
            })
    }

    pub unsafe fn push_string(&mut self, s: &str) {
        let v = PgsString::new(self, s).unwrap_or_else(|| {
            self.collect_garbage();
            PgsString::new(self, s).unwrap()
        });
        self.push(v)
    }

    pub unsafe fn push_symbol(&mut self, name: &str) {
        let v = Symbol::new(self, name).unwrap_or_else(|| {
            self.collect_garbage();
            Symbol::new(self, name).unwrap()
        });
        self.push(v)
    }

    pub unsafe fn cons(&mut self) {
        let mut pair = Pair::new(self).unwrap_or_else(|| {
            self.collect_garbage();
            Pair::new(self).unwrap()
        });
        pair.cdr = self.stack.pop().unwrap();
        pair.car = self.stack.pop().unwrap();
        self.push(pair);
    }

    pub unsafe fn push_vector(&mut self, len: Fixnum) {
        let vec = Vector::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Vector::new(self, len).unwrap()
        });
        self.push(vec);
    }

    pub unsafe fn vector(&mut self, len: Fixnum) {
        let mut vec = Vector::new(self, len).unwrap_or_else(|| {
            self.collect_garbage();
            Vector::new(self, len).unwrap()
        });
        let len: usize = len.into();
        vec.copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.push(vec)
    }

    pub unsafe fn closure(&mut self, code: Primop, len: Fixnum) {
        let mut f = Closure::new(self, code, len).unwrap_or_else(|| {
            self.collect_garbage();
            Closure::new(self, code, len).unwrap()
        });
        let len: usize = len.into();
        f.clovers_mut().copy_from_slice(&self.stack[self.stack.len() - len..]);
        self.stack.truncate(self.stack.len() - len);
        self.stack.push(f.into())
    }

    pub unsafe fn push_primitive(&mut self, code: Primop) {
        let f = Closure::new(self, code, 0.into()).unwrap_or_else(|| {
            self.collect_garbage();
            Closure::new(self, code, 0.into()).unwrap()
        });
        self.push(f);
    }

    pub unsafe fn push_syntax(&mut self, loc: Loc) -> Result<(), RuntimeError> {
        let datum = self.peek().unwrap();
        let line = loc.pos.line.try_into()?;
        let column = loc.pos.column.try_into()?;
        self.push(loc.source);
        let syntax = if let Some(syntax) =
            Syntax::new(self, datum, Value::FALSE, loc.source, line, column)
        {
            self.pop::<Value>().unwrap().unwrap(); // source
            self.pop::<Value>().unwrap().unwrap(); // datum
            syntax
        } else {
            self.collect_garbage();
            let source = self.pop().unwrap().unwrap();
            let datum = self.pop().unwrap().unwrap();
            Syntax::new(self, datum, Value::FALSE, source, line, column).unwrap()
        };
        self.push(syntax);
        Ok(())
    }

    pub unsafe fn make_type(&mut self, field_count: usize) -> Result<(), RuntimeError> {
        let is_bytes: bool = self.downcast(self.get(field_count + 3).unwrap())?;
        let is_flex: bool = self.downcast(self.get(field_count + 2).unwrap())?;
        let name: Symbol = self.downcast(self.get(field_count + 1).unwrap())?;
        let parent = self.get(field_count).unwrap();
        let parent =
            if parent == Value::FALSE { None } else { Some(self.downcast::<Type>(parent)?.into()) };

        if !is_bytes {
            if !is_flex {
                let min_size = (field_count * size_of::<Value>()).try_into()?;

                let t = Type::new(
                    self,
                    is_bytes,
                    is_flex,
                    name,
                    parent,
                    min_size,
                    transmute::<&[Value], &[FieldDescriptor]>(
                        &self.stack[self.stack.len() - field_count..]
                    ) // FIXME
                )
                .unwrap_or_else(|| {
                    self.collect_garbage();

                    let name = Symbol::unchecked_downcast(self.get(field_count - 1).unwrap());
                    let parent = self.get(field_count).unwrap();
                    let parent = if parent == Value::FALSE { None } else { Some(parent) };

                    Type::new(
                        self,
                        is_bytes,
                        is_flex,
                        name,
                        parent,
                        min_size,
                        transmute::<&[Value], &[FieldDescriptor]>(
                            &self.stack[self.stack.len() - field_count..]
                        ) // FIXME
                    )
                    .unwrap()
                });
                self.stack.truncate(self.stack.len() - field_count - 4);
                self.push(t);
                Ok(())
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    pub unsafe fn make(&mut self, len: usize) -> Result<(), RuntimeError> {
        let t: Type = self.downcast(self.get(len).unwrap())?;

        if t.is_bytes == Value::FALSE {
            if t.is_flex == Value::FALSE {
                assert!(len == t.fields().len());

                let mut res = HeapValue::<()>::unchecked_downcast(
                    self.heap.alloc(Object::new(t), len * size_of::<Value>()).unwrap_or_else(
                        || {
                            self.collect_garbage();
                            let t = Type::unchecked_downcast(self.get(len).unwrap());
                            self.heap.alloc(Object::new(t), len * size_of::<Value>()).unwrap()
                        }
                    )
                );
                res.slots_mut().copy_from_slice(&self.stack[self.stack.len() - len..]);
                self.stack.truncate(self.stack.len() - len - 1);
                self.push(res);
                Ok(())
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    pub fn get_symbol(&mut self, name: &str) -> Option<Symbol> {
        let symbol_t = self.types().symbol;
        self.symbol_table.get(&mut self.heap, symbol_t, name)
    }

    // TODO: Get print names straight
    pub unsafe fn gensym(&mut self, name: Symbol) {
        let hash = name.hash;
        let symbol_t = self.types().symbol;
        let s =
            Symbol::create(&mut self.heap, symbol_t, hash, name.as_str()).unwrap_or_else(|| {
                self.push(name);
                self.collect_garbage();
                let name = Symbol::unchecked_downcast(self.pop().unwrap().unwrap());
                let symbol_t = self.types().symbol;
                Symbol::create(&mut self.heap, symbol_t, hash, name.as_str()).unwrap()
            });
        self.push(s);
    }

    pub fn push_env(&mut self) { self.push(self.env) }

    pub fn env(&self) -> Bindings { self.env }

    pub fn set_env(&mut self, env: Bindings) { self.env = env }

    pub unsafe fn push_scope(&mut self) {
        let env = Bindings::new(self, Some(self.env)).unwrap_or_else(|| {
            self.collect_garbage();
            Bindings::new(self, Some(self.env)).unwrap()
        });
        self.env = env;
    }

    pub fn lookup(&mut self) -> Result<(), RuntimeError> {
        let name = unsafe { Symbol::unchecked_downcast(self.pop().unwrap().unwrap()) }; // checked before call
        self.env.get(self, name).map(|v| self.push(v)).ok_or(RuntimeError::Unbound(name))
    }

    pub unsafe fn define(&mut self) -> Result<(), RuntimeError> {
        let value = self.pop().unwrap().unwrap();
        let name = self.pop().unwrap()?;
        self.env.insert(self, name, value).unwrap_or_else(|_| {
            self.push(name);
            self.push(value);
            self.collect_garbage();
            let value = self.pop().unwrap().unwrap();
            let name = self.pop().unwrap().unwrap();
            self.env.insert(self, name, value).unwrap()
        });
        Ok(())
    }

    pub fn set(&mut self) -> Result<(), RuntimeError> {
        let value = self.pop().unwrap()?;
        let name = self.pop().unwrap()?;
        self.env.set(self, name, value).map_err(|()| RuntimeError::Unbound(name))?;
        Ok(self.stack.push(Value::UNSPECIFIED))
    }

    pub fn raise<'a, T, E: Into<PgsError>>(&mut self, err: E) -> Result<T, PgsError> {
        // TODO: Actually try raising Scheme exception.
        Err(err.into())
    }

    pub fn unwind(&mut self) {
        // TODO: Also unwind env to top level
        self.stack.truncate(0);
    }

    // TODO: References from symbol table should be weak
    pub unsafe fn collect_garbage(&mut self) {
        self.heap
            .collection()
            .roots(self.stack.iter_mut().map(|v| v as *mut Value))
            .roots(iter::once(transmute::<&mut Bindings, *mut Value>(&mut self.env)))
            .roots(self.immediate_types.iter_mut().map(|v| v as *mut Value))
            .roots(self.object_types.indexed.iter_mut().map(|v| v as *mut Value))
            .traverse()
            .weaks(self.symbol_table.iter_mut().map(|v| transmute::<&mut Symbol, *mut Value>(v)));
    }

    pub unsafe fn dump<W: io::Write>(&self, dest: &mut W) -> io::Result<()> {
        writeln!(dest, "environment:\n")?;
        self.env.dump(self, dest)?;

        writeln!(dest, "")?;

        writeln!(dest, "stack trace:\n")?;
        self.print_stacktrace(dest)?;

        Ok(())
    }

    pub unsafe fn print_stacktrace<W: io::Write>(&self, dest: &mut W) -> io::Result<()> {
        for (tag, slots) in self.stack_frames() {
            writeln!(dest, "{:?}", tag)?;

            for slot in slots.iter().rev() {
                writeln!(dest, "\t{}", slot.fmt_wrap(self))?;
            }
        }

        Ok(())
    }

    unsafe fn stack_frames(&self) -> Frames {
        Frames { stack: &self.stack, index: self.stack.len() - 1, done: self.stack.len() < 1 }
    }

    pub fn resolve_path(&mut self, filename: &str) -> Option<PathBuf> {
        fn step(dir: &Path, filename: &str) -> Option<PathBuf> {
            let mut path = PathBuf::new();
            path.push(dir);
            path.push(filename);
            if path.exists() {
                Some(path)
            } else {
                None
            }
        }

        let ores = step(&env::current_dir().unwrap(), filename);
        if ores.is_some() {
            return ores;
        }

        let include_path_sym = self.get_symbol("*include-path*")?;
        let mut include_path = self.env.get(self, include_path_sym)?;

        while let Ok(path_pair) = self.downcast::<Pair>(include_path) {
            let dir =
                self.downcast::<PgsString>(path_pair.car).expect("non-string in *include-path*"); // HACK

            let ores = step(dir.as_ref(), filename);
            if ores.is_some() {
                return ores;
            } else {
                include_path = path_pair.cdr;
            }
        }

        None
    }
}

struct Frames<'a> {
    stack: &'a [Value],
    index: usize,
    done: bool
}

impl<'a> Iterator for Frames<'a> {
    type Item = (FrameTag, &'a [Value]);

    fn next(&mut self) -> Option<Self::Item> {
        if !self.done {
            loop {
                if unimplemented!()
                /* self.stack[self.index].is_frame_tag() */
                {
                    break;
                } else if self.index == 0 {
                    self.done = true;
                    return None;
                } else {
                    self.index -= 1;
                }
            }

            let i = self.index;
            let tag = unsafe { transmute::<Value, FrameTag>(self.stack[i]) };
            let (base_len, dynamic) = tag.framesize();
            let len = if dynamic {
                let dyn_len: usize = todo!();
                // self.stack[i - 2].try_into().expect("Stack frame length corrupted");
                base_len + dyn_len + 1
            } else {
                base_len
            };
            if len + 1 >= i {
                self.done = true;
            } else {
                self.index -= len + 1;
            }
            Some((tag, &self.stack[i - len..i]))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let n = 4i16;
        for i in 0..n {
            state.push(i);
        }
        state.push(Value::NIL);
        for _ in 0..n {
            unsafe {
                state.cons();
            }
        }

        unsafe {
            state.collect_garbage();
        }
        unsafe {
            state.collect_garbage();
        } // overwrite initial heap and generally cause more havoc

        let mut ls = state.pop().unwrap().unwrap();
        for i in 0..n {
            let pair: Pair = state.downcast(ls).unwrap();
            assert_eq!(pair.car, i.into());
            ls = pair.cdr;
        }
        assert_eq!(ls, Value::NIL);
    }
}

use std::cell::UnsafeCell;
use std::collections::hash_map::RandomState;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{BuildHasher, Hash, Hasher};
use std::io;
use std::mem::{self, align_of, size_of, swap, transmute};
use std::ops::{Deref, DerefMut};
use std::slice;
use std::str;

use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

use super::gc::{HeapObject, MemoryManager, ObjectReference};
use super::refs::{DynamicDowncast, DynamicType, Fixnum, HeapValue, Primop, UnpackedValue, Value};
use super::state::{self, State};
use super::util::{False, True};

// ---

pub enum UnpackedHeapValue {
    Vector(Vector),
    String(PgsString),
    Symbol(Symbol),
    Pair(Pair),
    Bindings(Bindings),
    Closure(Closure),
    Syntax(Syntax),
    Type(Type),
    FieldDescriptor(FieldDescriptor),
    Other(Value)
}

impl Display for UnpackedHeapValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            UnpackedHeapValue::String(s) => write!(f, "{}", s),
            UnpackedHeapValue::Symbol(s) => write!(f, "{}", s),
            UnpackedHeapValue::Pair(p) => p.fmt(f),
            UnpackedHeapValue::Vector(v) => v.fmt(f),
            UnpackedHeapValue::Closure(proc) => write!(f, "{}", proc),
            UnpackedHeapValue::Bindings(env) => write!(f, "{}", env),
            UnpackedHeapValue::Syntax(s) => s.fmt(f),
            UnpackedHeapValue::Type(t) => write!(f, "{}", t),
            UnpackedHeapValue::FieldDescriptor(fd) => write!(f, "{}", fd),
            UnpackedHeapValue::Other(v) =>
                write!(f, "#<instance {}>", state::with(|state| state.type_of(v))),
        }
    }
}

// ---

#[derive(Debug, Clone, Copy)]
pub struct Header(Value);

// FIXME: Actually VM local, move to State
thread_local! {
    static IDENTITY_HASHES: UnsafeCell<SmallRng> = UnsafeCell::new(SmallRng::from_entropy());
}

impl Header {
    const BAD_HASH: usize = 0xf0;

    fn new() -> Self { Self(unsafe { transmute::<usize, Value>(Self::BAD_HASH) }) }

    pub fn from_hash(hash: usize) -> Self { Self(Value::try_from(hash).unwrap()) }

    fn identity_hash(&mut self) -> usize {
        if self.0 != unsafe { transmute::<usize, Value>(Self::BAD_HASH) } {
            unsafe { Fixnum::unchecked_downcast(self.0).into() }
        } else {
            let mut hash = IDENTITY_HASHES.with(|id_hashes| unsafe { (*id_hashes.get()).gen() });
            hash = hash << Value::SHIFT;

            if hash == Self::BAD_HASH {
                hash = 0xbad0;
            }
            self.0 = unsafe { transmute::<usize, Value>(hash) };
            hash >> Value::SHIFT
        }
    }

    fn forward(&mut self, data: *const u8) { self.0 = unsafe { Value::from_ptr(data as *mut u8) }; }

    fn forwarded(self) -> Option<Value> { HeapValue::<()>::try_from(self.0).ok().map(Value::from) }
}

// ---

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Object {
    pub header: Header,
    pub typ: Type
}

impl Object {
    pub fn new(typ: Type) -> Self { Self { header: Header::new(), typ } }

    pub fn is_bytes(&self) -> bool { self.typ.is_bytes != Value::FALSE }

    pub fn typ(&self) -> Type { self.typ }

    pub fn len(&self) -> usize { self.typ.instance_len(self) }

    pub fn identity_hash(&mut self) -> usize { self.header.identity_hash() }

    pub fn flex_length(&self) -> Option<Fixnum> { self.typ.instance_flex_length(self) }

    pub fn flex_fields(&self) -> Option<&[Value]> { self.typ.instance_flex_fields(self) }

    pub fn flex_fields_mut(&mut self) -> Option<&mut [Value]> {
        let typ = self.typ;
        typ.instance_flex_fields_mut(self)
    }

    unsafe fn flex_slots(&self) -> &[Value] { self.typ.instance_flex_slots(self) }

    unsafe fn flex_slots_mut(&mut self) -> &mut [Value] {
        let typ = self.typ;
        typ.instance_flex_slots_mut(self)
    }

    unsafe fn flex_bytes(&self) -> &[u8] { self.typ.instance_flex_bytes(self) }

    unsafe fn flex_bytes_mut(&mut self) -> &mut [u8] {
        let typ = self.typ;
        typ.instance_flex_bytes_mut(self)
    }
}

impl HeapObject for Object {
    type Ref = Value;
    type Fields = PtrFields;

    const LAPSED: Self::Ref = Value::ZERO; // This is fine since it's just for symbol table

    fn is_alignment_hole(mem: *const Self) -> bool {
        unsafe { transmute::<Type, usize>((*mem).typ) == 0 }
    }

    fn forward(&mut self, data: *const u8) { self.header.forward(data) }

    fn forwarded(&self) -> Option<Self::Ref> { self.header.forwarded() }

    fn size(&self) -> usize { self.typ.instance_size(self) }
    fn align(&self) -> usize { align_of::<Value>() }

    fn data(&mut self) -> *mut u8 { (unsafe { (self as *mut Self).offset(1) }) as *mut u8 }
    fn ptr_fields(&mut self) -> Self::Fields { PtrFields::new(self) }
}

#[derive(Debug)]
pub struct PtrFields {
    ptr: *mut Value,
    len: usize
}

impl PtrFields {
    fn new(obj: *mut Object) -> Self {
        unsafe {
            Self {
                // Type is also a pointer field
                ptr: ((*obj).data() as *mut Value).offset(-1),
                len: if (*obj).is_bytes() { 1 } else { (*obj).len() + 1 }
            }
        }
    }
}

impl Iterator for PtrFields {
    type Item = *mut Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len > 0 {
            let ptr = self.ptr;
            self.ptr = unsafe { ptr.add(1) };
            self.len -= 1;
            Some(ptr)
        } else {
            None
        }
    }
}

// ---

pub type Vector = HeapValue<VectorData>;

impl DynamicType for VectorData {
    type IsBytes = False;
    type IsFlex = True;

    fn reify_via(state: &State) -> Type { state.types().vector }
}

#[repr(C)]
pub struct VectorData;

impl Vector {
    pub fn new(state: &mut State, len: Fixnum) -> Option<Self> {
        state.alloc_flex::<VectorData>(len)
    }
}

impl Deref for Vector {
    type Target = [Value];

    fn deref(&self) -> &Self::Target { unsafe { (*self.header()).flex_slots() } }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { (*self.header_mut()).flex_slots_mut() }
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "#(")?;

        for (i, v) in self.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", v)?;
        }

        write!(f, ")")
    }
}

// ---

pub type PgsString = HeapValue<PgsStringData>;

impl DynamicType for PgsStringData {
    type IsBytes = True;
    type IsFlex = True;

    fn reify_via(state: &State) -> Type { state.types().string }
}

#[repr(C)]
pub struct PgsStringData;

impl PgsString {
    pub fn new(state: &mut State, cs: &str) -> Option<Self> {
        let len = cs.len();
        state.alloc_flex::<PgsStringData>(len.try_into().unwrap()).map(|res| {
            unsafe { (*res.header_mut()).flex_bytes_mut().copy_from_slice(cs.as_bytes()) };
            res
        })
    }
}

impl PgsString {
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked((*self.header()).flex_bytes()) }
    }
}

impl Deref for PgsString {
    type Target = str;

    fn deref(&self) -> &Self::Target { self.as_str() }
}

impl Debug for PgsString {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{:?}", self.as_str()) }
}

impl Display for PgsString {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "\"{}\"", self.as_str()) }
}

// ---

#[derive(Debug)]
#[repr(C)]
pub struct SymbolData {
    pub hash: u64
}

pub type Symbol = HeapValue<SymbolData>;

impl DynamicType for SymbolData {
    type IsBytes = True;
    type IsFlex = True;

    fn reify_via(state: &State) -> Type { state.types().symbol }
}

impl Deref for Symbol {
    type Target = SymbolData;

    fn deref(&self) -> &Self::Target { unsafe { &*(self.data() as *const Self::Target) } }
}

impl DerefMut for Symbol {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.data() as *mut Self::Target) }
    }
}

impl Symbol {
    pub fn new(state: &mut State, name: &str) -> Option<Self> { state.get_symbol(name) }

    pub fn create(
        heap: &mut MemoryManager<Object>, symbol_t: Type, hash: u64, name: &str
    ) -> Option<Self> {
        heap.alloc(
            Object::new(symbol_t),
            size_of::<SymbolData>() + size_of::<Fixnum>() + name.len()
        )
        .map(|res| unsafe {
            let mut res = Symbol::unchecked_downcast(res);
            res.hash = hash;
            *((&mut *res as *mut SymbolData).add(1) as *mut Fixnum) =
                name.len().try_into().unwrap();
            (*res.header_mut()).flex_bytes_mut().copy_from_slice(name.as_bytes());
            res
        })
    }
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked((*self.header()).flex_bytes()) }
    }
}

pub struct SymbolTable {
    symbols: Vec<Value>,
    occupancy: usize,
    hash_builder: RandomState
}

impl SymbolTable {
    const VACANT: Value = Value::FALSE;
    const TOMBSTONE: Value = Value::ZERO;

    pub fn new() -> Self {
        Self { symbols: vec![Self::VACANT; 2], occupancy: 0, hash_builder: RandomState::new() }
    }

    /// Only returns `None` if symbol was not found and then allocating it
    /// failed.
    pub fn get(
        &mut self, heap: &mut MemoryManager<Object>, symbol_t: Type, name: &str
    ) -> Option<Symbol> {
        self.ensure_vacancy();

        let hash = self.hash_key(name);

        match self.locate(name, hash) {
            Ok(symbol) => Some(symbol),
            Err(vacancy) => Symbol::create(heap, symbol_t, hash, name).map(|symbol| {
                self.symbols[vacancy] = symbol.into();
                self.occupancy += 1;
                symbol
            })
        }
    }

    pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Symbol> {
        self.symbols.iter_mut().filter_map(|v| match *v {
            Self::TOMBSTONE => None,
            Self::VACANT => None,
            _ => Some(unsafe { transmute::<&mut Value, &mut Symbol>(v) })
        })
    }

    fn locate(&self, k: &str, hash: u64) -> Result<Symbol, usize> {
        let capacity = self.symbols.len();

        for collisions in 0..capacity {
            let i = hash as usize + collisions & capacity - 1; // hash + collisions % capacity

            match self.symbols[i] {
                Self::TOMBSTONE => {},
                Self::VACANT => return Err(i),
                symbol => {
                    let symbol = unsafe { Symbol::unchecked_downcast(symbol) };
                    if symbol.hash == hash && symbol.as_str() == k {
                        return Ok(symbol);
                    }
                }
            }
        }
        unreachable!() // If we got here this was called on a full table
    }

    // OPTIMIZE: If there are lots of tombstones, don't need to grow
    fn ensure_vacancy(&mut self) {
        if self.occupancy + 1 > self.symbols.len() >> 1 {
            // new_load_factor > 0.5
            self.rehash();
        }
    }

    fn rehash(&mut self) {
        fn insert(this: &mut SymbolTable, symbol: Symbol) {
            let hash = symbol.hash;
            let capacity = this.symbols.len();

            for collisions in 0..capacity {
                let i = hash as usize + collisions & capacity - 1; // hash + collisions % capacity

                match this.symbols[i] {
                    SymbolTable::TOMBSTONE => unreachable!(), // in rehashed, nothing is deleted
                    SymbolTable::VACANT => {
                        this.symbols[i] = symbol.into();
                        return;
                    },
                    _ => continue // we are rehashing, cannot be equal to `symbol`
                }
            }
            unreachable!() // If we got here this was called on a full table
        }

        let mut symbols = vec![Self::VACANT; self.symbols.len() << 1];
        mem::swap(&mut self.symbols, &mut symbols);

        for v in symbols {
            match v {
                Self::TOMBSTONE => self.occupancy -= 1,
                Self::VACANT => {},
                symbol => insert(self, unsafe { Symbol::unchecked_downcast(symbol) })
            }
        }
    }

    fn hash_key(&self, k: &str) -> u64 {
        let mut hasher = self.hash_builder.build_hasher();
        k.hash(&mut hasher);
        hasher.finish()
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{}", self.as_str()) }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{}", self.as_str()) }
}

// ---

pub type Pair = HeapValue<PairData>;

impl DynamicType for PairData {
    type IsBytes = False;
    type IsFlex = False;

    fn reify_via(state: &State) -> Type { state.types().pair }
}

#[repr(C)]
pub struct PairData {
    pub car: Value,
    pub cdr: Value
}

impl Pair {
    pub fn new(state: &mut State) -> Option<Self> { state.alloc::<PairData>() }
}

impl Display for Pair {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}", self.car)?;

        let mut ls = self.cdr;

        while let Ok(p) = Pair::try_from(ls) {
            write!(f, " {}", p.car)?;
            ls = p.cdr;
        }

        if ls != Value::NIL {
            write!(f, " . {}", ls)?;
        }

        write!(f, ")")
    }
}

// ---

pub type Closure = HeapValue<ClosureData>;

impl DynamicType for ClosureData {
    type IsBytes = False;
    type IsFlex = True;

    fn reify_via(state: &State) -> Type { state.types().procedure }
}

impl Deref for Closure {
    type Target = ClosureData;

    fn deref(&self) -> &Self::Target { unsafe { &*(self.data() as *const Self::Target) } }
}

impl DerefMut for Closure {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.data() as *mut Self::Target) }
    }
}

#[repr(C)]
pub struct ClosureData {
    pub code: Primop
}

impl Closure {
    pub fn new(state: &mut State, code: Primop, clover_count: Fixnum) -> Option<Self> {
        state.alloc_flex::<ClosureData>(clover_count).map(|mut res| {
            res.code = code;
            res
        })
    }

    pub fn clovers(&self) -> &[Value] { unsafe { (*self.header()).flex_slots() } }

    pub fn clovers_mut(&mut self) -> &mut [Value] {
        unsafe { (*self.header_mut()).flex_slots_mut() }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "#<procedure>") }
}

// ---

pub type Bindings = HeapValue<BindingsData>;

impl DynamicType for BindingsData {
    type IsBytes = False;
    type IsFlex = False;

    fn reify_via(state: &State) -> Type { state.types().environment }
}

#[repr(C)]
pub struct BindingsData {
    parent: Value,
    keys: Vector,
    values: Vector,
    occupancy: Fixnum
}

impl Bindings {
    pub fn new(state: &mut State, parent: Option<Bindings>) -> Option<Bindings> {
        state.alloc::<BindingsData>().and_then(|mut bindings| {
            Vector::new(state, 2.into()).and_then(|keys| {
                Vector::new(state, 2.into()).map(|values| {
                    bindings.parent = parent.map_or(Value::FALSE, Value::from);
                    bindings.keys = keys; // keys are already VACANT (= 0) initialized
                    bindings.values = values;
                    // occupancy is already 0
                    bindings
                })
            })
        })
    }

    const VACANT: Value = Value::ZERO;

    pub fn get(self, name: Symbol) -> Option<Value> {
        let mut this = self;
        loop {
            match this.locate(name) {
                (i, true) => return Some(this.values[i]),
                (_, false) =>
                    if let Ok(parent) = Bindings::try_from(this.parent) {
                        this = parent;
                    } else {
                        return None;
                    },
            }
        }
    }

    // Returns `Err` when had to grow and allocation failed.
    pub fn insert(self, state: &mut State, name: Symbol, value: Value) -> Result<(), ()> {
        self.ensure_vacancy(state).map(|_| self.insert_noresize(name, value)).ok_or(())
    }

    // Returns `Err` if not found.
    pub fn set(self, name: Symbol, value: Value) -> Result<(), ()> {
        let mut this = self;
        loop {
            match this.locate(name) {
                (i, true) => {
                    this.values[i] = value;
                    return Ok(());
                },
                (_, false) =>
                    if let Ok(parent) = Bindings::try_from(this.parent) {
                        this = parent;
                    } else {
                        return Err(());
                    },
            }
        }
    }

    fn insert_noresize(mut self, name: Symbol, value: Value) {
        let (i, _) = self.locate(name);
        self.keys[i] = name.into();
        self.values[i] = value;
        self.occupancy = (self.occupancy + Fixnum::from(1)).unwrap();
    }

    fn locate(self, name: Symbol) -> (usize, bool) {
        let hash = name.hash as usize;
        let capacity = self.capacity();

        for collisions in 0..capacity {
            let i = hash + collisions & capacity - 1; // hash + collisions % capacity

            match self.keys[i] {
                Self::VACANT => return (i, false),
                k =>
                    if k == Value::from(name) {
                        return (i, true);
                    },
            }
        }
        unreachable!() // If we got here this was called on a full table
    }

    fn ensure_vacancy(self, state: &mut State) -> Option<()> {
        if <Fixnum as Into<usize>>::into(self.occupancy) + 1 > self.capacity() / 2 {
            self.rehash(state)
        } else {
            Some(())
        }
    }

    fn rehash(mut self, state: &mut State) -> Option<()> {
        let len = (2 * self.keys.len()).try_into().unwrap();

        Vector::new(state, len).and_then(|mut keys| {
            Vector::new(state, len).map(|mut values| {
                swap(&mut self.keys, &mut keys);
                swap(&mut self.values, &mut values);

                for (i, &key) in keys.iter().enumerate() {
                    match key {
                        Self::VACANT => {},
                        name => self
                            .insert_noresize(unsafe { Symbol::unchecked_downcast(name) }, values[i])
                    }
                }
            })
        })
    }

    fn capacity(self) -> usize { self.keys.len() }

    pub fn dump<W: io::Write>(self, state: &State, dest: &mut W) -> io::Result<()> {
        if let Ok(parent) = Bindings::try_from(self.parent) {
            parent.dump(state, dest)?;
        }

        for (k, v) in self.keys.iter().zip(self.values.iter()) {
            if *k != Self::VACANT {
                writeln!(dest, "{} = {}", k, v)?;
            }
        }

        Ok(())
    }
}

impl Display for Bindings {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "#<environment>") }
}

// ---

pub type Syntax = HeapValue<SyntaxObject>;

impl DynamicType for SyntaxObject {
    type IsBytes = False;
    type IsFlex = False;

    fn reify_via(state: &State) -> Type { state.types().syntax }
}

#[repr(C)]
pub struct SyntaxObject {
    pub datum: Value,
    pub scopes: Value,
    pub source: Value,
    pub line: Value,
    pub column: Value
}

impl Syntax {
    pub fn new(
        state: &mut State, datum: Value, scopes: Value, source: Value, line: Value, column: Value
    ) -> Option<Self> {
        state.alloc::<SyntaxObject>().map(|mut syn| {
            syn.datum = datum;
            syn.scopes = scopes;
            syn.source = source;
            syn.line = line;
            syn.column = column;
            syn
        })
    }
}

impl Value {
    /// `syntax->datum`
    pub unsafe fn to_datum(self, state: &mut State) -> Value {
        match self.unpack() {
            UnpackedValue::ORef(o) => match o.unpack() {
                UnpackedHeapValue::Syntax(syntax) => syntax.datum.to_datum(state),
                UnpackedHeapValue::Pair(pair) => {
                    state.push(pair);
                    let car = pair.car.to_datum(state);
                    state.push(car);
                    let pair = transmute::<Value, Pair>(state.remove(1).unwrap());
                    let cdr = pair.cdr.to_datum(state);
                    state.push(cdr);
                    state.cons();
                    state.pop().unwrap().unwrap()
                },
                UnpackedHeapValue::Vector(vec) => {
                    let len = vec.len();
                    state.push(vec);

                    for i in 0..len {
                        let vec = transmute::<Value, Vector>(state.get(i).unwrap());
                        let v = vec[i].to_datum(state);
                        state.push(v);
                    } // { vec v{len} }

                    state.vector(transmute::<usize, Fixnum>(len << Value::SHIFT)); // { vec vec* }
                    state.remove(1).unwrap();
                    state.pop().unwrap().unwrap()
                },
                _ => self
            },
            _ => self
        }
    }
}

impl Display for Syntax {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "#<syntax @ {}:{}:{} {}>", self.source, self.line, self.column, self.datum)
    }
}

// ---

pub type Type = HeapValue<TypeData>;

impl DynamicType for TypeData {
    type IsBytes = False;
    type IsFlex = True;

    fn reify_via(state: &State) -> Type { state.types().typ }
}

#[derive(Debug)]
#[repr(C)]
pub struct TypeData {
    pub is_bytes: Value,
    pub is_flex: Value,
    pub name: Symbol,
    pub parent: Value,
    pub min_size: Fixnum
}

impl Type {
    pub fn new(
        state: &mut State, is_bytes: bool, is_flex: bool, name: Symbol, parent: Option<Value>,
        min_size: Fixnum, fields: &[FieldDescriptor]
    ) -> Option<Self> {
        state.alloc_flex::<TypeData>(fields.len().try_into().unwrap()).map(|mut res| {
            res.is_bytes = is_bytes.into();
            res.is_flex = is_flex.into();
            res.name = name;
            res.parent = parent.unwrap_or(Value::FALSE);
            res.min_size = min_size;
            res.fields_mut().copy_from_slice(fields);
            res
        })
    }
}

impl TypeData {
    fn field_count(&self) -> usize {
        unsafe { (*((self as *const TypeData).add(1) as *const Fixnum)).into() }
    }

    pub fn fields(&self) -> &[FieldDescriptor] {
        unsafe {
            let len_ptr = (self as *const TypeData).add(1) as *const Fixnum;
            slice::from_raw_parts(len_ptr.add(1) as *const FieldDescriptor, (*len_ptr).into())
        }
    }

    pub fn fields_mut(&mut self) -> &mut [FieldDescriptor] {
        unsafe {
            let len_ptr = (self as *mut TypeData).add(1) as *mut Fixnum;
            slice::from_raw_parts_mut(len_ptr.add(1) as *mut FieldDescriptor, (*len_ptr).into())
        }
    }

    // TODO: Figure out bytes instances with fixed fields
    fn instance_len(&self, instance: &Object) -> usize {
        let min_len = self.field_count();

        if self.is_flex != Value::FALSE {
            let flex_len: usize = unsafe {
                (*((instance as *const Object).add(1) as *const Fixnum).add(min_len - 1)).into()
            };
            min_len + flex_len
        } else {
            min_len
        }
    }

    // TODO: Figure out bytes instances with fixed fields
    fn instance_size(&self, instance: &Object) -> usize {
        if self.is_flex != Value::FALSE {
            let min_size: usize = self.min_size.into();

            let flex_len: usize = unsafe {
                (*(((instance as *const Object).add(1) as *const u8)
                    .add(min_size - size_of::<Fixnum>()) as *const Fixnum))
                    .into()
            };
            let flex_size = if self.is_bytes != Value::FALSE {
                flex_len
            } else {
                flex_len * size_of::<Value>()
            };

            min_size + flex_size
        } else {
            self.min_size.into()
        }
    }

    fn instance_flex_length(&self, instance: &Object) -> Option<Fixnum> {
        if self.is_flex != Value::FALSE {
            Some(unsafe { self.instance_flex_len(instance) })
        } else {
            None
        }
    }

    unsafe fn instance_flex_len(&self, instance: &Object) -> Fixnum {
        *(((instance as *const Object).add(1) as *const u8)
            .add(<Fixnum as Into<usize>>::into(self.min_size) - size_of::<Fixnum>())
            as *const Fixnum)
    }

    unsafe fn instance_flex_ptr(&self, instance: &Object) -> *const u8 {
        ((instance as *const Object).add(1) as *const u8)
            .add(<Fixnum as Into<usize>>::into(self.min_size))
    }

    fn instance_flex_fields(&self, instance: &Object) -> Option<&[Value]> {
        if self.is_flex != Value::FALSE {
            Some(unsafe { self.instance_flex_slots(instance) })
        } else {
            None
        }
    }

    fn instance_flex_fields_mut<'a>(&self, instance: &'a mut Object) -> Option<&'a mut [Value]> {
        if self.is_flex != Value::FALSE {
            Some(unsafe { self.instance_flex_slots_mut(instance) })
        } else {
            None
        }
    }

    unsafe fn instance_flex_slots(&self, instance: &Object) -> &[Value] {
        slice::from_raw_parts(
            self.instance_flex_ptr(instance) as *const Value,
            self.instance_flex_len(instance).into()
        )
    }

    unsafe fn instance_flex_slots_mut<'a>(&self, instance: &'a mut Object) -> &'a mut [Value] {
        slice::from_raw_parts_mut(
            self.instance_flex_ptr(instance) as *mut Value,
            self.instance_flex_len(instance).into()
        )
    }

    unsafe fn instance_flex_bytes(&self, instance: &Object) -> &[u8] {
        slice::from_raw_parts(
            self.instance_flex_ptr(instance),
            self.instance_flex_len(instance).into()
        )
    }

    unsafe fn instance_flex_bytes_mut<'a>(&self, instance: &'a mut Object) -> &'a mut [u8] {
        slice::from_raw_parts_mut(
            self.instance_flex_ptr(instance) as *mut u8,
            self.instance_flex_len(instance).into()
        )
    }
}

impl Deref for Type {
    type Target = TypeData;

    fn deref(&self) -> &Self::Target { unsafe { &*(self.data() as *const Self::Target) } }
}

impl DerefMut for Type {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.data() as *mut Self::Target) }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "{}", self) }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "#<type {}", self.name)?;

        for &field in self.fields() {
            write!(f, " {}", field)?;
        }

        write!(f, ">")
    }
}

// ---

pub type FieldDescriptor = HeapValue<FieldDescriptorData>;

#[repr(C)]
pub struct FieldDescriptorData {
    pub is_mutable: Value,
    pub size: Fixnum,
    pub name: Symbol
}

impl DynamicType for FieldDescriptorData {
    type IsBytes = False;
    type IsFlex = False;

    fn reify_via(state: &State) -> Type { state.types().field_descriptor }
}

impl FieldDescriptor {
    pub fn new(state: &mut State, is_mutable: bool, size: Fixnum, name: Symbol) -> Option<Self> {
        state.alloc::<FieldDescriptorData>().map(|mut res| {
            *res = FieldDescriptorData { is_mutable: is_mutable.into(), size, name };
            res
        })
    }
}

impl Display for FieldDescriptor {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mutability = if self.is_mutable == Value::TRUE { "mutable" } else { "immutable" };
        write!(f, "({} {} {})", mutability, self.name, self.size)
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryInto;

    #[test]
    fn test_vector() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let len = 7;
        let i = 3;

        let mut vec = Vector::new(&mut state, len.try_into().unwrap()).unwrap();

        assert_eq!(vec.len(), len);
        assert_eq!(vec[i], Value::from(0i16));

        vec[i] = Value::from('a');

        assert_eq!(vec[i], Value::from('a'));
    }

    #[test]
    fn test_string() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let cs = "foo";

        let s = PgsString::new(&mut state, cs).unwrap();

        assert_eq!(s.as_str(), cs);
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let name = "foo";

        let s = Symbol::new(&mut state, name).unwrap();
        let t = Symbol::new(&mut state, name).unwrap();

        assert_eq!(s.as_str(), name);
        assert!(s.hash != 0);
        assert_eq!(s.hash, t.hash);
        assert_eq!(Value::from(s), Value::from(t));
    }

    #[test]
    fn test_pair() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let a = Value::from(5i16);
        let b = Value::from(8i16);

        let mut p = Pair::new(&mut state).unwrap();
        p.car = a;
        p.cdr = b;

        assert_eq!(p.car, a);
        assert_eq!(p.cdr, b);
    }

    #[test]
    fn test_bindings() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let bindings = Bindings::new(&mut state, None).unwrap();
        let a = Value::from(5i16);
        let b = Value::from(8i16);
        let foo = unsafe {
            state.push_symbol("foo");
            state.pop().unwrap().unwrap()
        };
        let bar = unsafe {
            state.push_symbol("bar");
            state.pop().unwrap().unwrap()
        };

        bindings.insert(&mut state, foo, a).unwrap();
        bindings.insert(&mut state, bar, b).unwrap();

        assert_eq!(bindings.get(foo).unwrap(), a);
        assert_eq!(bindings.get(bar).unwrap(), b);
    }

    #[test]
    fn test_identity_hash() {
        let mut state = State::new(&[], 1 << 20, 1 << 20);
        let a = Value::from(5i16);
        let b = Value::from(8i16);
        let mut p = Pair::new(&mut state).unwrap();

        assert_eq!(a.identity_hash(), Value::from(5i16).identity_hash());

        let phash = p.identity_hash();
        assert!(phash != 0);
        p.car = a;
        p.cdr = b;
        assert_eq!(p.identity_hash(), phash);
    }
}

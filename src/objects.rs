use std::collections::hash_map::RandomState;
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher, BuildHasher};
use std::io;
use std::mem::{self, size_of, align_of, swap, transmute};
use std::ops::{Deref, DerefMut};
use std::slice;
use std::str;
use std::cell::UnsafeCell;

use rand::{SeedableRng, Rng};
use rand::rngs::SmallRng;

use super::gc::{HeapObject, MemoryManager};
use super::refs::{Value, HeapValue};
use super::state::State;

// ---

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum HeapTag {
    Pair = 0x0,
    Vector = 0x1,
    String = 0x2,
    Symbol = 0x3,
    Closure = 0x4,
    Bindings = 0x5
}

impl HeapTag {
    const FIRST_REFS: usize = Self::Pair as usize;

    fn is_bytes(self) -> bool { (self as usize) < Self::FIRST_REFS }
    
    fn skips(self) -> bool { self == Self::Closure }

    fn align(self) -> usize {
        if self.is_bytes() {
            if self == Self::Symbol {
                align_of::<SymbolData>()
            } else {
                align_of::<u8>()
            }
        } else {
            align_of::<Value>()
        }
    }
}

// ---

#[repr(usize)]
pub enum Code {
    ApplySelf = 0
}

impl TryFrom<usize> for Code {
    type Error = ();

    fn try_from(code: usize) -> Result<Self, Self::Error> {
        if code < 256 { // HACK
            Ok(unsafe { transmute::<usize, Self>(code) })
        } else {
            Err(())
        }
    }
}

// ---

pub trait Heaped {
    const TAG: HeapTag;
}

pub enum UnpackedHeapValue {
    Vector(Vector),
    String(PgsString),
    Symbol(Symbol),
    Pair(Pair),
    Bindings(Bindings),
    Closure(Closure)
}

impl Display for UnpackedHeapValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            UnpackedHeapValue::Vector(vec) => {
                "#(".fmt(f)?;

                for (i, v) in vec.iter().enumerate() {
                    if i > 0 {
                        " ".fmt(f)?;
                    }
                    v.fmt(f)?;
                }

                ")".fmt(f)
            },
            UnpackedHeapValue::String(s) => s.fmt(f),
            UnpackedHeapValue::Symbol(s) => s.fmt(f),
            UnpackedHeapValue::Pair(mut p) => {
                "(".fmt(f)?;

                loop {
                    p.car.fmt(f)?;

                    if let Ok(cdr) = Pair::try_from(p.cdr) {
                        " ".fmt(f)?;
                        p = cdr;
                    } else {
                        break;
                    }
                }

                if p.cdr != Value::NIL {
                    write!(f, " . {}", p.cdr)?;
                }

                ")".fmt(f)
            },
            UnpackedHeapValue::Bindings(_) => "#<environment>".fmt(f),
            UnpackedHeapValue::Closure(_) => "#<procedure>".fmt(f)
        }
    }
}

// ---

/// Bit 0 (LSB) is always set, for heap parsability (header vs. alignment padding).
/// Bit 1 is mark (forwarded) bit
/// Bit 2 is bytes bit
/// Bit 3 is skip (closure) bit
/// Bits 4-7 are type tag
/// Other bits are len
#[derive(Clone, Copy)]
pub struct Header(usize);

impl Header {
    const FWD_MASK: usize = 0b11;
    const FWD_TAG: usize = 0b11;
    const TYPE_SHIFT: usize = 4;
    const TYPE_MASK: usize = 0b1111;
    const SIZE_SHIFT: usize = 8;

    const BYTES_BIT: usize = 0b100;
    const SKIP_BIT: usize = 0b1000;

    pub fn new(type_tag: HeapTag, len: usize) -> Self {
        // FIXME: Check that `len` fits in 24 / 56 bits
        Self( len << Self::SIZE_SHIFT
            | (type_tag as usize) << Self::TYPE_SHIFT
            | (type_tag.skips() as usize) << 3
            | (type_tag.is_bytes() as usize) << 2
            | 0b01 )
    }

    fn is_alignment_hole(mem: *const Self) -> bool { unsafe { (*mem).0 == 0 } }

    fn len(&self) -> usize { self.0 >> Self::SIZE_SHIFT }

    fn size(&self) -> usize {
        let len = self.len();
        if self.is_bytes() {
            len
        } else {
            len * size_of::<Value>()
        }
    }

    fn tag(&self) -> HeapTag {
        unsafe { transmute((self.0 >> Self::TYPE_SHIFT & Self::TYPE_MASK) as u8) }
    }

    fn align(&self) -> usize {
        if self.is_bytes() {
            self.tag().align()
        } else {
            align_of::<Value>()
        }
    }

    fn is_bytes(&self) -> bool { self.0 & Self::BYTES_BIT == Self::BYTES_BIT }

    fn skips(&self) -> bool { self.0 & Self::SKIP_BIT == Self::SKIP_BIT }

    fn is_forwarding(&self) -> bool { self.0 & Self::FWD_MASK == Self::FWD_TAG }
 
    unsafe fn forwarding(ptr: *const u8) -> Self {
        Self(ptr as usize | Self::FWD_TAG)
    }

    fn forward(&self) -> Option<Value> {
        if self.is_forwarding() {
            Some(unsafe { Value::from_data((self.0 & !Self::FWD_MASK) as *mut u8) })
        } else {
            None
        }
    }
}

// ---

#[derive(Clone, Copy)]
pub struct Object {
    identity_hash: usize,
    header: Header
}

thread_local! {
    static IDENTITY_HASHES: UnsafeCell<SmallRng> = UnsafeCell::new(SmallRng::from_entropy());
}

impl Object {
    pub fn new(header: Header) -> Self { Self {identity_hash: 0, header} }

    pub fn tag(&self) -> HeapTag { self.header.tag() }

    pub fn is_bytes(&self) -> bool { self.header.is_bytes() }

    pub fn skips(&self) -> bool { self.header.skips() }

    pub fn len(&self) -> usize { self.header.len() }

    pub fn identity_hash(&mut self) -> usize {
        let mut hash = self.identity_hash;

        if hash != 0 {
             hash
        } else {
            hash = IDENTITY_HASHES.with(|id_hashes| unsafe { (*id_hashes.get()).gen() });
            if hash == 0 {
                hash = 0xbad;
            }
            self.identity_hash = hash;
            hash
        }
    }
}

impl HeapObject for Object {
    type Ref = Value;
    type Fields = PtrFields;

    const LAPSED: Self::Ref = Value::UNBOUND;

    fn is_alignment_hole(mem: *const Self) -> bool { Header::is_alignment_hole(unsafe{ &(*mem).header }) }

    unsafe fn forwarding(data: *const u8) -> Self { Self {identity_hash: 0, header: Header::forwarding(data)} }
    fn forward(&self) -> Option<Self::Ref> { self.header.forward() }

    fn size(&self) -> usize { self.header.size() }
    fn align(&self) -> usize { self.header.align() }

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
        let obj = unsafe { &mut *obj };
        // OPTIMIZE:
        Self {
            ptr: if obj.skips() {
                unsafe { (obj.data() as *mut Value).add(1) }
            } else {
                obj.data() as *mut Value
            },
            len: if obj.is_bytes() {
                0
            } else if obj.skips() {
                obj.len() - 1
            } else {
                obj.len()
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

#[repr(C)]
pub struct VectorData {
    items: [Value]
}

impl Heaped for VectorData {
    const TAG: HeapTag = HeapTag::Vector;
}

impl Vector {
    pub fn new(state: &mut State, len: usize) -> Option<Self> {
        state.alloc(Object::new(Header::new(HeapTag::Vector, len)))
    }
}

impl Deref for Vector {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        unsafe {
            slice::from_raw_parts(self.data() as *const Value, (*self.as_ptr()).len())
        }
    }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            slice::from_raw_parts_mut(self.data() as *mut Value, (*self.as_ptr()).len())
        }
    }
}

// ---

pub type PgsString = HeapValue<PgsStringData>;

#[repr(C)]
pub struct PgsStringData {
    chars: str
}

impl Heaped for PgsStringData {
    const TAG: HeapTag = HeapTag::String;
}

impl PgsString {
    pub fn new(state: &mut State, cs: &str) -> Option<Self> {
        let len = cs.len();
        state.alloc::<PgsStringData>(Object::new(Header::new(HeapTag::String, len))).map(|res| {
            let data = unsafe { slice::from_raw_parts_mut(res.data(), len) };
            data.copy_from_slice(cs.as_bytes());
            res
        })
    }
}

impl PgsString {
    pub fn as_str(&self) -> &str {
        unsafe {
            let bytes = slice::from_raw_parts(self.data(), (*self.as_ptr()).len());
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl Deref for PgsString {
    type Target = str;

    fn deref(&self) -> &Self::Target { self.as_str() }
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

impl Heaped for SymbolData {
    const TAG: HeapTag = HeapTag::Symbol;
}

pub type Symbol = HeapValue<SymbolData>;

impl Symbol {
    pub fn new(state: &mut State, name: &str) -> Option<Self> {
        state.get_symbol(name)
    }

    fn create(heap: &mut MemoryManager<Object>, hash: u64, name: &str) -> Option<Self> {
        let len = size_of::<SymbolData>() + name.len();
        heap.alloc(Object::new(Header::new(HeapTag::Symbol, len))).map(|res| {
            let mut res = unsafe { transmute::<Value, HeapValue<SymbolData>>(res) };

            res.hash = hash;
            let data = unsafe {
                let ptr = ((&mut *res) as *mut SymbolData).add(1) as *mut u8;
                slice::from_raw_parts_mut(ptr, name.len())
            };
            data.copy_from_slice(name.as_bytes());

            res
        })
    }
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        unsafe {
            let bytes = slice::from_raw_parts(self.data().add(size_of::<SymbolData>()),
                                              (*self.as_ptr()).len() - size_of::<SymbolData>());
            str::from_utf8_unchecked(bytes)
        }
    }
}

pub struct SymbolTable {
    symbols: Vec<Value>,
    occupancy: usize,
    hash_builder: RandomState
}

impl SymbolTable {
    const VACANT: Value = Value::FALSE;
    const TOMBSTONE: Value = Value::UNBOUND;

    pub fn new() -> Self {
        Self {
            symbols: vec![Self::VACANT; 2],
            occupancy: 0,
            hash_builder: RandomState::new()
        }
    }

    /// Only returns `None` if symbol was not found and then allocating it failed.
    pub fn get(&mut self, heap: &mut MemoryManager<Object>, name: &str) -> Option<Symbol> {
        self.ensure_vacancy();

        let hash = self.hash_key(name);

        match self.locate(name, hash) {
            Ok(symbol) => Some(symbol),
            Err(vacancy) => Symbol::create(heap, hash, name).map(|symbol| {
                self.symbols[vacancy] = symbol.into();
                self.occupancy += 1;
                symbol
            })
        }
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut Symbol> {
        self.symbols.iter_mut()
            .filter_map(|v| if let Ok(_) = Symbol::try_from(*v) {
                Some(unsafe { transmute::<&mut Value, &mut Symbol>(v) })
            } else {
                None
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
                    let symbol = unsafe { transmute::<Value, Symbol>(symbol) };
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
        if self.occupancy + 1 > self.symbols.len() >> 1 { // new_load_factor > 0.5
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
                symbol => insert(self, unsafe { transmute::<Value, Symbol>(symbol) })
            }
        }
    }

    fn hash_key(&self, k: &str) -> u64 {
        let mut hasher = self.hash_builder.build_hasher();
        k.hash(&mut hasher);
        hasher.finish()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { self.as_str().fmt(f) }
}

// ---

pub type Pair = HeapValue<PairData>;

#[repr(C)]
pub struct PairData {
    pub car: Value,
    pub cdr: Value
}

impl Heaped for PairData {
    const TAG: HeapTag = HeapTag::Pair;
}

impl Pair {
    pub fn new(state: &mut State) -> Option<Self> {
        state.alloc(Object::new(Header::new(HeapTag::Pair, size_of::<PairData>() / size_of::<Value>())))
    }

    pub fn cons(state: &mut State, car: Value, cdr: Value) -> Option<Self> {
        Self::new(state).map(|mut res| {
            *res = PairData {car, cdr};
            res
        })
    }
}

pub struct Cars(Value);

impl Cars {
    pub fn of(value: Value) -> Self { Self(value) }

    pub fn remainder(&self) -> Value { self.0 }
}

impl Iterator for Cars {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Ok(pair) = Pair::try_from(self.0) {
            self.0 = pair.cdr;
            Some(pair.car)
        } else {
            None
        }
    }
}

// ---

#[repr(C)]
pub struct ClosureData {
    pub code: usize
}

impl Heaped for ClosureData {
    const TAG: HeapTag = HeapTag::Closure;
}

pub type Closure = HeapValue<ClosureData>;

impl Closure {
    pub fn new(state: &mut State, code: usize, clover_count: usize) -> Option<Self> {
        let len = clover_count + 1;
        state.alloc::<ClosureData>(Object::new(Header::new(HeapTag::Closure, len))).map(|mut res| {
            res.code = code;
            res
        })
    }
}

impl Closure {
    pub fn clovers(&self) -> &[Value] {
        unsafe {
            slice::from_raw_parts((self.data() as *const Value).add(1), (*self.as_ptr()).len() - 1)
        }
    }

    pub fn clovers_mut(&mut self) -> &mut [Value] {
        unsafe {
            slice::from_raw_parts_mut((self.data() as *mut Value).add(1), (*self.as_ptr()).len() - 1)
        }
    }
}

// ---

#[repr(C)]
pub struct BindingsData {
    parent: Value,
    keys: Vector,
    values: Vector,
    occupancy: Value
}

impl Heaped for BindingsData {
    const TAG: HeapTag = HeapTag::Bindings;
}

pub type Bindings = HeapValue<BindingsData>;

impl Bindings {
    pub fn new(state: &mut State, parent: Option<Bindings>) -> Option<Bindings> {
        state.alloc::<BindingsData>(Object::new(Header::new(HeapTag::Bindings, size_of::<BindingsData>() / size_of::<Value>())))
            .and_then(|mut bindings| {
                Vector::new(state, 2).and_then(|keys| {
                    Vector::new(state, 2).map(|values| {
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
                (_, false) => if let Ok(parent) = Bindings::try_from(this.parent) {
                    this = parent;
                } else {
                    return None;
                }
            }
        }
    }

    // Returns `Err` when had to grow and allocation failed.
    pub fn insert(self, state: &mut State, name: Symbol, value: Value) -> Result<(), ()> {
        self.ensure_vacancy(state)
            .map(|_| self.insert_noresize(name, value))
            .ok_or(())
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
                (_, false) => if let Ok(parent) = Bindings::try_from(this.parent) {
                    this = parent;
                } else {
                    return Err(())
                }
            }
        }
    }

    fn insert_noresize(mut self, name: Symbol, value: Value) {
        let (i, _) = self.locate(name);
        self.keys[i] = name.into();
        self.values[i] = value;
        unsafe {
            self.occupancy = transmute::<usize, Value>(transmute::<Value, usize>(self.occupancy) + (1 << Value::SHIFT));
        }
    }

    fn locate(self, name: Symbol) -> (usize, bool) {
        let hash = name.hash as usize;
        let capacity = self.capacity();

        for collisions in 0..capacity {
            let i = hash + collisions & capacity - 1; // hash + collisions % capacity

            match self.keys[i] {
                Self::VACANT => return (i, false),
                k => if k == Value::from(name) {
                    return (i, true);
                }
            }
        }
        unreachable!() // If we got here this was called on a full table
    }

    fn ensure_vacancy(self, state: &mut State) -> Option<()> {
        if unsafe { transmute::<Value, usize>(self.occupancy) >> Value::SHIFT } + 1 > self.capacity() / 2 {
            self.rehash(state)
        } else {
            Some(())
        }
    }

    fn rehash(mut self, state: &mut State) -> Option<()> {
        let len = 2 * self.keys.len();

        Vector::new(state, len).and_then(|mut keys| {
            Vector::new(state, len).map(|mut values| {
                swap(&mut self.keys, &mut keys);
                swap(&mut self.values, &mut values);

                for (i, &key) in keys.iter().enumerate() {
                    match key {
                        Self::VACANT => {},
                        name => self.insert_noresize(unsafe { transmute(name) }, values[i])
                    }
                }
            })
        })
    }

    fn capacity(self) -> usize { self.keys.len() }

    pub fn dump<W: io::Write>(self, dest: &mut W) -> io::Result<()> {
        for (k, v) in self.keys.iter().zip(self.values.iter()) {
            if *k != Self::VACANT {
                writeln!(dest, "{} = {}", k, v)?;
            }
        }

        Ok(())
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    use std::convert::TryInto;

    #[test]
    fn test_vector() {
        let mut state = State::new(1 << 12, 1 << 20);
        let len = 7;
        let i = 3;
        
        let mut vec = Vector::new(&mut state, len).unwrap();

        assert_eq!(vec.len(), len);
        assert_eq!(vec[i], Value::from(0i16));

        vec[i] = Value::from('a');

        assert_eq!(vec[i], Value::from('a'));
    }

    #[test]
    fn test_string() {
        let mut state = State::new(1 << 12, 1 << 20);
        let cs = "foo";

        let s = PgsString::new(&mut state, cs).unwrap();
        
        assert_eq!(s.as_str(), cs);
    }

    #[test]
    fn test_symbol() {
        let mut state = State::new(1 << 12, 1 << 20);
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
        let mut state = State::new(1 << 12, 1 << 20);
        let a = Value::from(5i16);
        let b = Value::from(8i16);

        let p = Pair::cons(&mut state, a, b).unwrap();

        assert_eq!(p.car, a);
        assert_eq!(p.cdr, b);
    }

    #[test]
    fn test_bindings() {
        let mut state = State::new(1 << 12, 1 << 20);
        let bindings = Bindings::new(&mut state, None).unwrap();
        let a = Value::from(5i16);
        let b = Value::from(8i16);
        let foo = unsafe {state.push_symbol("foo"); state.pop().unwrap().try_into().unwrap()};
        let bar = unsafe {state.push_symbol("bar"); state.pop().unwrap().try_into().unwrap()};

        bindings.insert(&mut state, foo, a).unwrap();
        bindings.insert(&mut state, bar, b).unwrap();

        assert_eq!(bindings.get(foo).unwrap(), a);
        assert_eq!(bindings.get(bar).unwrap(), b);
    }

    #[test]
    fn test_identity_hash() {
        let mut state = State::new(1 << 12, 1 << 20);
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


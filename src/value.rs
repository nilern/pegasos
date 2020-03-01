use std::char;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, size_of, align_of, transmute};
use std::ops::{Deref, DerefMut};
use std::slice;
use std::str;

use super::util::fsize;
use super::gc::{ObjectReference, HeapObject};
use super::state::State; // TODO: Break import cycle

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Value(usize);

impl ObjectReference for Value {
    type Object = Object;

    unsafe fn from_ptr(ptr: *mut Self::Object) -> Self {
        Self((*ptr).data() as usize | BaseTag::ORef as usize)
    }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.is_oref() {
            Some(unsafe { ((self.0 & !Self::MASK) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

#[derive(PartialEq)]
enum BaseTag {
    ORef = 0b01,
    Fixnum = 0b00, // i30 | i62
    Flonum = 0b10, // f30 | f62
    Ext = 0b11     // char, bool, '() etc.
}

#[derive(PartialEq)]
enum Tag {
    ORef = 0b01,
    Fixnum = 0b00,
    Flonum = 0b10,
    Char = 0b0111, // (28 | 60) bit char
    Bool = 0b1011,
    // 0b0011 is unused
    Singleton = 0b1111 // '() etc.
}

pub enum UnpackedValue {
    ORef(HeapValue<()>),
    Fixnum(isize),
    Flonum(fsize),
    Char(char),
    Bool(bool),
    Nil,
    Undefined,
    Eof
}

impl Value {
    const SHIFT: usize = 2;
    const TAG_COUNT: usize = 1 << Self::SHIFT;
    const MASK: usize = Self::TAG_COUNT - 1;

    const EXT_SHIFT: usize = 4;
    const EXT_MASK: usize = (1 << Self::EXT_SHIFT) - 1;

    pub const TRUE: Self = Self(1 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const FALSE: Self = Self(0 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const NIL: Self = Self(0 << Self::EXT_SHIFT | Tag::Singleton as usize); // '()
    pub const UNDEFINED: Self = Self(1 << Self::EXT_SHIFT | Tag::Singleton as usize); // for 'unspecified' stuff
    pub const EOF: Self = Self(2 << Self::EXT_SHIFT | Tag::Singleton as usize); // #!eof

    const BOUNDS_SHIFT: usize = 8 * size_of::<Self>() - Self::SHIFT; // 30/62

    fn base_tag(self) -> BaseTag { unsafe { transmute((self.0 & Self::MASK) as u8) } }

    fn tag(self) -> Tag {
        let base_tag = self.base_tag();
        if BaseTag::Ext == base_tag {
            unsafe { transmute((self.0 & Self::EXT_MASK) as u8) }
        } else {
            unsafe { transmute(base_tag) }
        }
    }

    fn is_oref(self) -> bool { self.base_tag() == BaseTag::ORef }

    pub fn unpack(self) -> UnpackedValue {
        match self.tag() {
            Tag::ORef => UnpackedValue::ORef(HeapValue {value: self, _phantom: PhantomData}),
            Tag::Fixnum => UnpackedValue::Fixnum((self.0 >> Self::SHIFT) as isize),
            Tag::Flonum => unimplemented!(),
            Tag::Char => UnpackedValue::Char(unsafe { char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32) }),
            Tag::Bool => UnpackedValue::Bool((self.0 >> Self::EXT_SHIFT) != 0),
            Tag::Singleton => match self {
                Self::NIL => UnpackedValue::Nil,
                _ => unimplemented!()
            }
        }
    }
}

impl TryFrom<isize> for Value {
    type Error = (); // FIXME

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0
           || n >> Self::BOUNDS_SHIFT == !0 as isize // fits in 30/62 bits, OPTIMIZE
        {
            Ok(Self((n << Self::SHIFT) as usize | BaseTag::Fixnum as usize))
        } else {
            Err(())
        }
    }
}

impl TryInto<isize> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<isize, Self::Error> {
        if self.base_tag() == BaseTag::Fixnum {
            Ok(self.0 as isize >> Self::SHIFT)
        } else {
            Err(())
        }
    }
}

impl TryFrom<fsize> for Value {
    type Error = (); // FIXME

    fn try_from(n: fsize) -> Result<Self, Self::Error> {
        if unimplemented!() { // fits in 30/62 bits
            Ok(Self(unsafe { mem::transmute::<_, usize>(n) } << Self::SHIFT | BaseTag::Flonum as usize))
        } else {
            Err(())
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::EXT_SHIFT | Tag::Char as usize) }
}

impl TryInto<char> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<char, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Char as usize {
            Ok(unsafe { char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32) })
        } else {
            Err(())
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::EXT_SHIFT | Tag::Bool as usize) }
}

impl TryInto<bool> for Value {
    type Error = (); // FIXME

    fn try_into(self) -> Result<bool, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Bool as usize {
            Ok(self.0 >> Self::EXT_SHIFT != 0)
        } else {
            Err(())
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use UnpackedValue::*;

        match self.unpack() {
            ORef(v) => v.fmt(f),
            Fixnum(n) => n.fmt(f), // HACK
            Flonum(n) => unimplemented!(),
            Char(c) => c.fmt(f), // HACK
            Bool(true) => "#true".fmt(f),
            Bool(false) => "#false".fmt(f),
            Nil => "()".fmt(f),
            _ => unimplemented!()
        }
    }
}

// ---

/// Bit 0 (LSB) is always set, for heap parsability (header vs. alignment padding).
/// Bit 1 is mark (forwarded) bit
/// Bit 2 is bytes bit
/// Bit 3 is unused
/// Bits 4-7 are type tag
/// Other bits are len
#[derive(Clone, Copy)]
struct Header(usize);

impl Header {
    const FWD_MASK: usize = 0b11;
    const FWD_TAG: usize = 0b11;
    const TYPE_SHIFT: usize = 4;
    const TYPE_MASK: usize = 0b1111;
    const SIZE_SHIFT: usize = 8;

    const BYTES_BIT: usize = 0b10;

    fn new(type_tag: HeapTag, len: usize) -> Self {
        // FIXME: Check that `len` fits in 24 / 56 bits
        Self(len << Self::SIZE_SHIFT
            | (type_tag as usize) << Self::TYPE_SHIFT
            | (type_tag.is_bytes() as usize) << 2
            | 0b01)
    }

    fn is_alignment_hole(mem: *const Self) -> bool { unsafe { (*mem).0 == 0 } }

    fn data(&self) -> *mut u8 { unsafe { (self as *const Self).add(1) as *mut u8 } }

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

    fn is_forwarding(&self) -> bool { self.0 & Self::FWD_MASK == Self::FWD_TAG }
 
    unsafe fn forwarding(oref: Value) -> Self {
        Self((oref.0 & !Value::MASK) | Self::FWD_TAG)
    }

    fn forward(&self) -> Option<Value> {
        if self.is_forwarding() {
            Some(Value((self.0 & !Self::FWD_MASK) | BaseTag::ORef as usize))
        } else {
            None
        }
    }
}

// ---

#[derive(Clone, Copy)]
pub struct Object {
    header: Header
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
enum HeapTag {
    String = 0x0,
    Symbol = 0x1,
    Pair = 0x2,
    Vector = 0x3
}

impl HeapTag {
    const FIRST_REFS: usize = Self::Pair as usize;

    fn is_bytes(self) -> bool { (self as usize) < Self::FIRST_REFS }

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

impl Object {
    fn tag(&self) -> HeapTag { self.header.tag() }

    fn is_bytes(&self) -> bool { self.header.is_bytes() }

    fn len(&self) -> usize { self.header.len() }
}

impl HeapObject for Object {
    type Ref = Value;
    type Fields = PtrFields;

    fn is_alignment_hole(mem: *const Self) -> bool { Header::is_alignment_hole(unsafe{ &(*mem).header }) }

    unsafe fn forwarding(oref: Self::Ref) -> Self { Self {header: Header::forwarding(oref)} }
    fn forward(&self) -> Option<Self::Ref> { self.header.forward() }

    fn size(&self) -> usize { self.header.size() }
    fn align(&self) -> usize { self.header.align() }

    fn data(&mut self) -> *mut u8 { (unsafe { (self as *mut Self).offset(1) }) as *mut u8 }
    fn ptr_fields(&mut self) -> Self::Fields { PtrFields::new(self) }
}

pub struct PtrFields {
    ptr: *mut Value,
    len: usize
}

impl PtrFields {
    fn new(obj: *mut Object) -> Self {
        let obj = unsafe { &mut *obj };
        Self {
            ptr: obj.data() as *mut Value,
            len: if obj.is_bytes() { 0 } else { obj.len() }
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

pub struct HeapValue<T> {
    value: Value,
    _phantom: PhantomData<*mut T>
}

pub enum UnpackedHeapValue {
    String(PgsString),
    Symbol(Symbol),
    Pair(Pair)
}

impl<T> Clone for HeapValue<T> {
    fn clone(&self) -> Self { Self {value: self.value, _phantom: self._phantom} }
}

impl<T> Copy for HeapValue<T> {}

impl<T> HeapValue<T> {
    fn heap_tag(self) -> HeapTag { unsafe { (*self.as_ptr()).tag() } }

    fn as_ptr(self) -> *mut Object {
        unsafe { (self.data() as *mut Object).offset(-1) }
    }

    fn data(self) -> *mut u8 { (self.value.0 & !Value::MASK) as *mut u8 }
}

impl HeapValue<()> {
    pub fn unpack(self) -> UnpackedHeapValue {
        match self.heap_tag() {
            HeapTag::String => UnpackedHeapValue::String(PgsString(HeapValue {value: self.value, _phantom: PhantomData})),
            HeapTag::Symbol => UnpackedHeapValue::Symbol(Symbol(HeapValue {value: self.value, _phantom: PhantomData})),
            HeapTag::Pair => UnpackedHeapValue::Pair(Pair(HeapValue {value: self.value, _phantom: PhantomData})),
            _ => unimplemented!()
        }
    }
}

impl<T> Deref for HeapValue<T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target { unsafe{ &*(self.data() as *const T) } }
}

impl<T> DerefMut for HeapValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut*(self.data() as *mut T) } }
}

impl<T> From<HeapValue<T>> for Value {
    fn from(v: HeapValue<T>) -> Self { v.value }
}

impl TryFrom<Value> for HeapValue<()> {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.base_tag() == BaseTag::ORef {
            Ok(Self {value, _phantom: PhantomData})
        } else {
            Err(())
        }
    }
}

impl Display for HeapValue<()> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.unpack() {
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
            }
        }
    }
}

// ---

#[derive(Clone, Copy)]
pub struct PgsString(HeapValue<u8>);

impl PgsString {
    pub fn new(state: &mut State, cs: &str) -> Option<Self> {
        let len = cs.len();
        let base = Object {header: Header::new(HeapTag::String, len)};
        state.alloc(base).map(|mut res| {
            let data = unsafe { slice::from_raw_parts_mut(&mut *res, len) };
            data.copy_from_slice(cs.as_bytes());

            PgsString(res)
        })
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            let obj = &mut *self.0.as_ptr();
            let bytes = slice::from_raw_parts(obj.data(), obj.len());
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl Display for PgsString {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "\"{}\"", self.as_str()) }
}

impl From<PgsString> for Value {
    fn from(s: PgsString) -> Self { s.0.into() }
}

impl TryFrom<Value> for PgsString {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        HeapValue::try_from(value).and_then(|oref| {
            if oref.heap_tag() == HeapTag::String {
                Ok(Self(HeapValue {value: oref.value, _phantom: PhantomData}))
            } else {
                Err(())
            }
        })
    }
}

// ---

#[derive(Clone, Copy)]
pub struct Symbol(HeapValue<SymbolData>);

pub struct SymbolData {
    pub hash: u64
}

impl Symbol {
    pub fn new(state: &mut State, name: &str) -> Option<Self> {
        // FIXME: Intern / hash cons
        let len = size_of::<SymbolData>() + name.len();
        let base = Object {header: Header::new(HeapTag::Symbol, len)};
        state.alloc::<SymbolData>(base).map(|mut res| {
            res.hash = {
                let mut hasher = DefaultHasher::new();
                HeapTag::Symbol.hash(&mut hasher);
                name.hash(&mut hasher);
                hasher.finish()
            };
            
            let data = unsafe {
                let ptr = ((&mut *res) as *mut SymbolData).add(1) as *mut u8;
                slice::from_raw_parts_mut(ptr, name.len())
            };
            data.copy_from_slice(name.as_bytes());

            Symbol(res)
        })
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            let obj = &mut *self.0.as_ptr();
            let bytes = slice::from_raw_parts(obj.data().add(size_of::<SymbolData>()),
                                              obj.len() - size_of::<SymbolData>());
            str::from_utf8_unchecked(bytes)
        }
    }
}

impl Deref for Symbol {
    type Target = SymbolData;

    fn deref(&self) -> &Self::Target { &*self.0 }
}

impl From<Symbol> for Value {
    fn from(symbol: Symbol) -> Self { symbol.0.into() }
}

impl TryFrom<HeapValue<()>> for Symbol {
    type Error = (); // FIXME

    fn try_from(v: HeapValue<()>) -> Result<Self, Self::Error> {
        if v.heap_tag() == HeapTag::Symbol {
            Ok(Self(HeapValue {value: v.value, _phantom: PhantomData}))
        } else {
            Err(())
        }
    }
}

impl TryFrom<Value> for Symbol {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        HeapValue::try_from(value).and_then(Self::try_from)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { self.as_str().fmt(f) }
}

// ---

#[derive(Clone, Copy)]
pub struct Pair(HeapValue<PairData>);

#[repr(C)]
pub struct PairData {
    pub car: Value,
    pub cdr: Value
}

impl Pair {
    pub fn new(state: &mut State) -> Option<Self> {
        let base = Object {header: Header::new(HeapTag::Pair, size_of::<PairData>() / size_of::<Value>())};
        state.alloc(base).map(Self)
    }

    pub fn cons(state: &mut State, car: Value, cdr: Value) -> Option<Self> {
        Self::new(state).map(|mut res| {
            *res = PairData {car, cdr};
            res
        })
    }
}

impl Deref for Pair {
    type Target = PairData;

    fn deref(&self) -> &Self::Target { &*self.0 }
}

impl DerefMut for Pair {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut *self.0 }
}

impl From<Pair> for Value {
    fn from(pair: Pair) -> Self { Value::from(pair.0) }
}

impl TryFrom<Value> for Pair {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        HeapValue::try_from(value).and_then(|hv| {
            if hv.heap_tag() == HeapTag::Pair {
                Ok(Self(HeapValue {value, _phantom: PhantomData}))
            } else {
                Err(())
            }
        })
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let c = 'a';

        let v = Value::from(c);

        assert!(!v.is_oref());
        assert_eq!(c, v.try_into().unwrap());
    }

    #[test]
    fn test_bool() {
        let b = true;

        let v = Value::from(b);

        assert!(!v.is_oref());
        assert_eq!(b, v.try_into().unwrap());
    }

    #[test]
    fn test_fixnum() {
        let n = 23;

        let v = Value::try_from(n).unwrap();

        assert!(!v.is_oref());
        assert_eq!(n, v.try_into().unwrap());

        let m = -n;

        let u = Value::try_from(m).unwrap();

        assert!(!u.is_oref());
        assert_eq!(m, u.try_into().unwrap());
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
        let hash = {
            let mut hasher = DefaultHasher::new();
            HeapTag::Symbol.hash(&mut hasher);
            name.hash(&mut hasher);
            hasher.finish()
        };

        let s = Symbol::new(&mut state, name).unwrap();
       
        assert_eq!(s.hash, hash);
        assert_eq!(s.as_str(), name);
    }

    #[test]
    fn test_pair() {
        let mut state = State::new(1 << 12, 1 << 20);
        let a = Value::try_from(5).unwrap();
        let b = Value::try_from(8).unwrap();

        let p = Pair::cons(&mut state, a, b).unwrap();

        assert_eq!(p.car, a);
        assert_eq!(p.cdr, b);
    }
}


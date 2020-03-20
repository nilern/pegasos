use std::char;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, size_of, transmute};
use std::ops::{Deref, DerefMut};
use std::slice;

use strum_macros::EnumIter;

use super::gc::{HeapObject, ObjectReference};
use super::interpreter::RuntimeError;
use super::objects::{BuiltInType, HeapTag, Heaped, Object, UnpackedHeapValue};
use super::util::fsize;

// ---

// TODO: Singletons could just as well be heap objects since there is only one
// allocation to pay for. 32-bit might have a somewhat bad time but if you are
// still on 32-bit you already have a bad time, anyway.

#[derive(Debug, PartialEq)]
pub enum Tag {
    Fixnum = 0, // i29 | i61, 0 => less fiddly arithmetic
    ORef = 1,   // the 1 should often sink into addressing modes
    Flonum = 2, // f29 | f61
    Char = 3,   // (28 | 60) bit char
    Bool = 4,
    Nil = 5,     // ()
    Unbound = 6, // Hash table sentinel
    Unspecified = 7
}

pub enum UnpackedValue {
    Fixnum(isize),
    ORef(HeapValue<()>),
    Flonum(fsize),
    Char(char),
    Bool(bool),
    Nil,
    Unbound,
    Unspecified
}

// ---

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(usize);

impl ObjectReference for Value {
    type Object = Object;

    unsafe fn from_ptr(data: *mut u8) -> Self { Self(data as usize | Tag::ORef as usize) }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.has_tag(Tag::ORef) {
            Some(unsafe { ((self.0 & !Self::MASK) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

impl Value {
    pub const SHIFT: usize = 3;
    const TAG_COUNT: usize = 1 << Self::SHIFT;
    const MASK: usize = Self::TAG_COUNT - 1;

    pub const ZERO: Self = Self(0 << Self::SHIFT | Tag::Fixnum as usize);

    pub const TRUE: Self = Self(1 << Self::SHIFT | Tag::Bool as usize);
    pub const FALSE: Self = Self(0 << Self::SHIFT | Tag::Bool as usize);

    pub const NIL: Self = Self(Tag::Nil as usize); // ()
    pub const UNBOUND: Self = Self(Tag::Unbound as usize); // 'tombstone'
    pub const UNSPECIFIED: Self = Self(Tag::Unspecified as usize); // for 'unspecified' stuff

    const BOUNDS_SHIFT: usize = 8 * size_of::<Self>() - Self::SHIFT; // 29 | 61

    pub fn tag(self) -> Tag { unsafe { transmute::<u8, Tag>((self.0 & Self::MASK) as u8) } }

    pub fn has_tag(self, tag: Tag) -> bool { self.tag() == tag }

    pub fn unpack(self) -> UnpackedValue {
        match self.tag() {
            Tag::Fixnum => UnpackedValue::Fixnum(self.0 as isize >> Self::SHIFT),
            Tag::ORef => UnpackedValue::ORef(HeapValue { value: self, _phantom: PhantomData }),
            Tag::Flonum =>
                UnpackedValue::Flonum(unsafe { transmute::<usize, fsize>(self.0 & !Self::MASK) }),
            Tag::Char => UnpackedValue::Char(unsafe {
                char::from_u32_unchecked((self.0 >> Self::SHIFT) as u32)
            }),
            Tag::Bool => UnpackedValue::Bool((self.0 >> Self::SHIFT) != 0),
            Tag::Nil => UnpackedValue::Nil,
            Tag::Unbound => UnpackedValue::Unbound,
            Tag::Unspecified => UnpackedValue::Unspecified
        }
    }

    pub fn identity_hash(self) -> usize {
        if let Ok(oref) = HeapValue::<()>::try_from(self) {
            oref.identity_hash()
        } else {
            let mut hasher = DefaultHasher::default();
            self.0.hash(&mut hasher);
            hasher.finish() as usize
        }
    }
}

impl From<u16> for Value {
    fn from(n: u16) -> Self { Self((n as usize) << Self::SHIFT | Tag::Fixnum as usize) }
}

impl From<i16> for Value {
    fn from(n: i16) -> Self { Self((n as usize) << Self::SHIFT | Tag::Fixnum as usize) }
}

impl TryFrom<isize> for Value {
    type Error = RuntimeError;

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0 || n >> Self::BOUNDS_SHIFT == !0 as isize
        // fits in 29 | 61 bits, OPTIMIZE
        {
            Ok(Self((n << Self::SHIFT) as usize | Tag::Fixnum as usize))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Fixnum))
        }
    }
}

impl TryInto<isize> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<isize, Self::Error> {
        if self.has_tag(Tag::Fixnum) {
            Ok(self.0 as isize >> Self::SHIFT)
        } else {
            Err(RuntimeError::Type { value: self, expected: BuiltInType::Fixnum })
        }
    }
}

impl TryFrom<usize> for Value {
    type Error = RuntimeError;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0 {
            // fits in 29 | 61 bits, OPTIMIZE
            Ok(Self(n << Self::SHIFT | Tag::Fixnum as usize))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Fixnum))
        }
    }
}

impl TryInto<usize> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<usize, Self::Error> {
        if self.has_tag(Tag::Fixnum) {
            Ok(self.0 >> Self::SHIFT)
        } else {
            Err(RuntimeError::Type { value: self, expected: BuiltInType::Fixnum })
        }
    }
}

impl TryFrom<fsize> for Value {
    type Error = RuntimeError;

    fn try_from(n: fsize) -> Result<Self, Self::Error> {
        if unimplemented!() {
            // fits in 29 | 61 bits
            Ok(Self(
                (unsafe { mem::transmute::<fsize, usize>(n) } & !Self::MASK) | Tag::Flonum as usize
            ))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Flonum))
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::SHIFT | Tag::Char as usize) }
}

impl TryInto<char> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<char, Self::Error> {
        if self.has_tag(Tag::Char) {
            Ok(unsafe { char::from_u32_unchecked((self.0 >> Self::SHIFT) as u32) })
        } else {
            Err(RuntimeError::Type { value: self, expected: BuiltInType::Char })
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::SHIFT | Tag::Bool as usize) }
}

impl TryInto<bool> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<bool, Self::Error> {
        if self.has_tag(Tag::Bool) {
            Ok(self.0 >> Self::SHIFT != 0)
        } else {
            Err(RuntimeError::Type { value: self, expected: BuiltInType::Char })
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { write!(f, "Value({:x})", self.0) }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use UnpackedValue::*;

        match self.unpack() {
            ORef(v) => Display::fmt(&v, f),
            Fixnum(n) => Display::fmt(&n, f), // HACK
            Flonum(n) => Display::fmt(&n, f), // HACK
            Char(c) => Display::fmt(&c, f),   // HACK
            Bool(true) => Display::fmt("#true", f),
            Bool(false) => Display::fmt("#false", f),
            Nil => Display::fmt("()", f),
            Unbound => Display::fmt("#<unbound>", f), // although we should never actually get here
            Unspecified => Display::fmt("#<unspecified>", f)
        }
    }
}

// ---

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(usize)]
pub enum Primop {
    Void = 0 << Value::SHIFT | Tag::Fixnum as usize,
    Eq = 1 << Value::SHIFT | Tag::Fixnum as usize,
    IdentityHash = 2 << Value::SHIFT | Tag::Fixnum as usize,
    Call = 3 << Value::SHIFT | Tag::Fixnum as usize,
    Apply = 4 << Value::SHIFT | Tag::Fixnum as usize,
    CallWithValues = 5 << Value::SHIFT | Tag::Fixnum as usize,
    Values = 6 << Value::SHIFT | Tag::Fixnum as usize,
    SymbolHash = 7 << Value::SHIFT | Tag::Fixnum as usize,
    ImmediateTypeIndex = 8 << Value::SHIFT | Tag::Fixnum as usize,
    HeapTypeIndex = 9 << Value::SHIFT | Tag::Fixnum as usize,
    Length = 10 << Value::SHIFT | Tag::Fixnum as usize,
    SlotRef = 11 << Value::SHIFT | Tag::Fixnum as usize,
    SlotSet = 12 << Value::SHIFT | Tag::Fixnum as usize,
    Record = 13 << Value::SHIFT | Tag::Fixnum as usize,
    Cons = 14 << Value::SHIFT | Tag::Fixnum as usize,
    Car = 15 << Value::SHIFT | Tag::Fixnum as usize,
    Cdr = 16 << Value::SHIFT | Tag::Fixnum as usize,
    MakeVector = 17 << Value::SHIFT | Tag::Fixnum as usize,
    VectorCopy = 18 << Value::SHIFT | Tag::Fixnum as usize,
    FxLt = 19 << Value::SHIFT | Tag::Fixnum as usize,
    FxAdd = 20 << Value::SHIFT | Tag::Fixnum as usize,
    FxSub = 21 << Value::SHIFT | Tag::Fixnum as usize,
    FxMul = 22 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseAnd = 23 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseIor = 24 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseXor = 25 << Value::SHIFT | Tag::Fixnum as usize,
    ArithmeticShift = 26 << Value::SHIFT | Tag::Fixnum as usize,
    BitCount = 27 << Value::SHIFT | Tag::Fixnum as usize,
    MakeSyntax = 28 << Value::SHIFT | Tag::Fixnum as usize,
    MakeType = 29 << Value::SHIFT | Tag::Fixnum as usize
}

impl Display for Primop {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Primop::*;

        write!(f, "##")?;
        match *self {
            Void => write!(f, "void"),
            Eq => write!(f, "eq?"),
            IdentityHash => write!(f, "identity-hash"),
            Call => write!(f, "call"),
            Apply => write!(f, "apply"),
            CallWithValues => write!(f, "call-with-values"),
            Values => write!(f, "values"),
            SymbolHash => write!(f, "symbol-hash"),
            ImmediateTypeIndex => write!(f, "immediate-tag"),
            HeapTypeIndex => write!(f, "heap-tag"),
            Length => write!(f, "object-length"),
            SlotRef => write!(f, "slot-ref"),
            SlotSet => write!(f, "slot-set!"),
            Record => write!(f, "record"),
            Cons => write!(f, "cons"),
            Car => write!(f, "car"),
            Cdr => write!(f, "cdr"),
            MakeVector => write!(f, "make-vector"),
            VectorCopy => write!(f, "vector-copy!"),
            FxLt => write!(f, "fx<?"),
            FxAdd => write!(f, "fx+"),
            FxSub => write!(f, "fx-"),
            FxMul => write!(f, "fx*"),
            BitwiseAnd => write!(f, "bitwise-and"),
            BitwiseIor => write!(f, "bitwise-ior"),
            BitwiseXor => write!(f, "bitwise-xor"),
            ArithmeticShift => write!(f, "arithmetic-shift"),
            BitCount => write!(f, "bit-count"),
            MakeSyntax => write!(f, "make-syntax"),
            MakeType => write!(f, "make-type")
        }
    }
}

// ---

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum FrameTag {
    Done = 0 << Value::SHIFT | Tag::Fixnum as usize,
    CondBranch = 1 << Value::SHIFT | Tag::Fixnum as usize,
    Define = 2 << Value::SHIFT | Tag::Fixnum as usize,
    Set = 3 << Value::SHIFT | Tag::Fixnum as usize,
    Let = 4 << Value::SHIFT | Tag::Fixnum as usize,
    Arg = 5 << Value::SHIFT | Tag::Fixnum as usize,
    Stmt = 6 << Value::SHIFT | Tag::Fixnum as usize,
    CallWithValues = 7 << Value::SHIFT | Tag::Fixnum as usize
}

impl FrameTag {
    pub fn framesize(self) -> (usize, bool) {
        use FrameTag::*;

        match self {
            Done => (1, false),
            CondBranch => (3, false),
            Define => (2, false),
            Set => (2, false),
            Let => (4, false),
            Arg => (2, true),
            Stmt => (2, false),
            CallWithValues => (2, false)
        }
    }
}

impl From<FrameTag> for Value {
    fn from(tag: FrameTag) -> Value { unsafe { transmute(tag) } }
}

// ---

pub struct HeapValue<T: ?Sized> {
    pub value: Value,
    pub _phantom: PhantomData<*mut T>
}

impl<T: ?Sized> Clone for HeapValue<T> {
    fn clone(&self) -> Self { Self { value: self.value, _phantom: self._phantom } }
}

impl<T: ?Sized> Copy for HeapValue<T> {}

impl<T> PartialEq for HeapValue<T> {
    fn eq(&self, other: &Self) -> bool { self.value.eq(&other.value) }
}

impl<T: ?Sized> HeapValue<T> {
    pub fn heap_tag(self) -> HeapTag { unsafe { (*self.as_ptr()).tag() } }

    pub fn as_ptr(self) -> *mut Object { unsafe { (self.data() as *mut Object).offset(-1) } }

    pub fn data(self) -> *mut u8 { (self.value.0 & !Value::MASK) as *mut u8 }

    pub fn slots<'a>(&'a self) -> &'a [Value] {
        unsafe {
            let obj = &mut *self.as_ptr();
            slice::from_raw_parts(
                obj.data() as *mut Value,
                if obj.is_bytes() { 0 } else { obj.len() }
            )
        }
    }

    pub fn slots_mut<'a>(&'a mut self) -> &'a mut [Value] {
        unsafe {
            let obj = &mut *self.as_ptr();
            slice::from_raw_parts_mut(
                obj.data() as *mut Value,
                if obj.is_bytes() { 0 } else { obj.len() }
            )
        }
    }

    pub fn identity_hash(self) -> usize { unsafe { (*self.as_ptr()).identity_hash() } }
}

impl HeapValue<()> {
    pub fn unpack(self) -> UnpackedHeapValue {
        match self.heap_tag() {
            HeapTag::Vector =>
                UnpackedHeapValue::Vector(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::String =>
                UnpackedHeapValue::String(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Symbol =>
                UnpackedHeapValue::Symbol(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Pair =>
                UnpackedHeapValue::Pair(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Bindings =>
                UnpackedHeapValue::Bindings(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Closure =>
                UnpackedHeapValue::Closure(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Syntax =>
                UnpackedHeapValue::Syntax(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Record =>
                UnpackedHeapValue::Record(HeapValue { value: self.value, _phantom: PhantomData }),
            HeapTag::Type =>
                UnpackedHeapValue::Type(HeapValue { value: self.value, _phantom: PhantomData }),
        }
    }
}

impl<T> Deref for HeapValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { unsafe { &*(self.data() as *const T) } }
}

impl<T> DerefMut for HeapValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut *(self.data() as *mut T) } }
}

impl<T: ?Sized> From<HeapValue<T>> for Value {
    fn from(v: HeapValue<T>) -> Self { v.value }
}

impl TryFrom<Value> for HeapValue<()> {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.has_tag(Tag::ORef) {
            Ok(Self { value, _phantom: PhantomData })
        } else {
            Err(())
        }
    }
}

impl<T: Heaped + ?Sized> TryFrom<Value> for HeapValue<T> {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Ok(oref) = HeapValue::<()>::try_from(value) {
            if oref.heap_tag() == T::TAG {
                Ok(HeapValue { value: oref.value, _phantom: PhantomData })
            } else {
                Err(RuntimeError::Type { value, expected: T::TYPE })
            }
        } else {
            Err(RuntimeError::Type { value, expected: T::TYPE })
        }
    }
}

impl<T: Debug> Debug for HeapValue<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { <T as Debug>::fmt(&*self, f) }
}

impl Display for HeapValue<()> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { self.unpack().fmt(f) }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let c = 'a';

        let v = Value::from(c);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(c, v.try_into().unwrap());
    }

    #[test]
    fn test_bool() {
        let b = true;

        let v = Value::from(b);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(b, v.try_into().unwrap());
    }

    #[test]
    fn test_fixnum() {
        let n = 23isize;

        let v = Value::from(23i16);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(n, v.try_into().unwrap());

        let m = -n;

        let u = Value::try_from(m).unwrap();

        assert!(!u.has_tag(Tag::ORef));
        assert_eq!(m, u.try_into().unwrap());
    }
}

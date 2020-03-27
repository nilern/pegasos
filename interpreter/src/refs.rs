use std::char;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{size_of, transmute};
use std::ops::{Add, BitAnd, BitOr, BitXor, Deref, DerefMut, Mul, Neg, Shl, Shr, Sub};
use std::slice;

use strum_macros::EnumIter;

use super::gc::{HeapObject, ObjectReference};
use super::interpreter::RuntimeError;
use super::objects::{
    Bindings, Closure, Object, Pair, PgsString, Symbol, Syntax, Type, UnpackedHeapValue, Vector
};
use super::state::State;
use super::util::{fsize, Bool, False};

// ---

pub trait DynamicDowncast: TryInto<Value> {
    fn downcast(state: &State, v: Value) -> Result<Self, RuntimeError>;

    unsafe fn unchecked_downcast(v: Value) -> Self;
}

pub trait DynamicType: Sized {
    type IsBytes: Bool;
    type IsFlex: Bool;

    fn reify(state: &State) -> Type;
}

impl<T: DynamicType> DynamicDowncast for HeapValue<T> {
    fn downcast(state: &State, v: Value) -> Result<Self, RuntimeError> {
        if state.type_of(v) == T::reify(state) {
            Ok(unsafe { Self::unchecked_downcast(v) })
        } else {
            Err(RuntimeError::Type { expected: T::reify(state), value: v })
        }
    }

    unsafe fn unchecked_downcast(value: Value) -> Self {
        HeapValue { value, _phantom: PhantomData }
    }
}

// ---

// TODO: Singletons could just as well be heap objects since there is only one
// allocation to pay for. 32-bit might have a somewhat bad time but if you are
// still on 32-bit you already have a bad time, anyway.

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
#[repr(usize)]
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

impl Display for Tag {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "<{}>", match *self {
            Tag::Fixnum => "fixnum",
            Tag::ORef => "oref",
            Tag::Flonum => "flonum",
            Tag::Char => "char",
            Tag::Bool => "boolean",
            Tag::Nil => "null",
            Tag::Unbound => "unbound",
            Tag::Unspecified => "unspecified"
        })
    }
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

    unsafe fn from_ptr(data: *mut u8) -> Self { Self(data as usize + Tag::ORef as usize) }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.has_tag(Tag::ORef) {
            Some(unsafe { ((self.0 - Tag::ORef as usize) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

impl DynamicDowncast for Value {
    fn downcast(_: &State, v: Value) -> Result<Self, RuntimeError> { Ok(v) }

    unsafe fn unchecked_downcast(v: Value) -> Self { v }
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

    pub fn tag(self) -> Tag { unsafe { transmute::<usize, Tag>(self.0 & Self::MASK) } }

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
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl DynamicDowncast for isize {
    fn downcast(state: &State, value: Value) -> Result<Self, RuntimeError> {
        state.downcast::<Fixnum>(value).map(|n| n.into())
    }

    unsafe fn unchecked_downcast(v: Value) -> Self { v.0 as isize >> Value::SHIFT }
}

impl TryFrom<usize> for Value {
    type Error = RuntimeError;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0 {
            // fits in 29 | 61 bits, OPTIMIZE
            Ok(Self(n << Self::SHIFT | Tag::Fixnum as usize))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl DynamicDowncast for usize {
    fn downcast(state: &State, value: Value) -> Result<Self, RuntimeError> {
        state.downcast::<Fixnum>(value).map(|n| n.into())
    }

    unsafe fn unchecked_downcast(v: Value) -> Self { v.0 >> Value::SHIFT }
}

impl TryFrom<fsize> for Value {
    type Error = RuntimeError;

    fn try_from(n: fsize) -> Result<Self, Self::Error> {
        if unimplemented!() {
            // fits in 29 | 61 bits
            Ok(Self((unsafe { transmute::<fsize, usize>(n) } & !Self::MASK) | Tag::Flonum as usize))
        } else {
            Err(RuntimeError::FlonumOverflow)
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::SHIFT | Tag::Char as usize) }
}

impl DynamicDowncast for char {
    fn downcast(state: &State, value: Value) -> Result<Self, RuntimeError> {
        if value.has_tag(Tag::Char) {
            Ok(unsafe { Self::unchecked_downcast(value) })
        } else {
            Err(RuntimeError::Type {
                expected: unsafe {
                    Type::unchecked_downcast(state.immediate_types()[Tag::Char as usize])
                },
                value
            })
        }
    }

    unsafe fn unchecked_downcast(v: Value) -> Self {
        char::from_u32_unchecked((v.0 >> Value::SHIFT) as u32)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::SHIFT | Tag::Bool as usize) }
}

impl DynamicDowncast for bool {
    fn downcast(state: &State, v: Value) -> Result<Self, RuntimeError> {
        if v.has_tag(Tag::Bool) {
            Ok(unsafe { Self::unchecked_downcast(v) })
        } else {
            Err(RuntimeError::Type {
                value: v,
                expected: unsafe {
                    Type::unchecked_downcast(state.immediate_types()[Tag::Bool as usize])
                }
            })
        }
    }

    unsafe fn unchecked_downcast(v: Value) -> Self { v.0 >> Value::SHIFT != 0 }
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

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
pub struct Fixnum(Value);

impl Fixnum {
    const TAG: Tag = Tag::Fixnum;
    const WIDTH: usize = 8 * size_of::<Self>() - Value::SHIFT;

    pub fn count_ones(self) -> Fixnum { Fixnum::from((self.0).0.count_ones() as u16) }
}

impl From<u16> for Fixnum {
    fn from(n: u16) -> Self { Self(Value((n as usize) << Value::SHIFT)) }
}

impl TryFrom<usize> for Fixnum {
    type Error = RuntimeError;

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        if n >> Value::BOUNDS_SHIFT == 0 {
            // fits in 29 | 61 bits, OPTIMIZE
            Ok(Self(Value(n << Value::SHIFT | Tag::Fixnum as usize)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl From<Fixnum> for Value {
    fn from(n: Fixnum) -> Value { n.0 }
}

impl DynamicDowncast for Fixnum {
    fn downcast(state: &State, v: Value) -> Result<Self, RuntimeError> {
        if v.tag() == Self::TAG {
            Ok(Self(v))
        } else {
            Err(RuntimeError::Type {
                expected: unsafe {
                    Type::unchecked_downcast(state.immediate_types()[Tag::Fixnum as usize])
                },
                value: v
            })
        }
    }

    unsafe fn unchecked_downcast(v: Value) -> Self { Self(v) }
}

impl Into<isize> for Fixnum {
    fn into(self) -> isize { ((self.0).0 as isize) >> Value::SHIFT }
}

impl Into<usize> for Fixnum {
    fn into(self) -> usize { (self.0).0 >> Value::SHIFT }
}

impl Add for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        if let Some(res) = ((self.0).0 as isize).checked_add((rhs.0).0 as isize) {
            Ok(Self(Value(res as usize)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl Sub for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        if let Some(res) = ((self.0).0 as isize).checked_sub((rhs.0).0 as isize) {
            Ok(Self(Value(res as usize)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl Mul for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        if let Some(res) = ((self.0).0 as isize).checked_mul((rhs.0).0 as isize >> Value::SHIFT) {
            Ok(Self(Value(res as usize)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl Neg for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn neg(self) -> Self::Output {
        if let Some(res) = ((self.0).0 as isize).checked_neg() {
            Ok(Self(Value(res as usize)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl BitAnd for Fixnum {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output { Self(Value((self.0).0 & (rhs.0).0)) }
}

impl BitOr for Fixnum {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output { Self(Value((self.0).0 | (rhs.0).0)) }
}

impl BitXor for Fixnum {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output { Self(Value((self.0).0 ^ (rhs.0).0)) }
}

#[cfg(target_pointer_width = "64")]
impl Shl for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn shl(self, rhs: Self) -> Self::Output {
        let shift: isize = rhs.into();

        if 0 <= shift && shift < Self::WIDTH as isize {
            let n: i128 = ((self.0).0 as isize as i128) << shift;
            if let Ok(n) = isize::try_from(n) {
                Ok(Self(Value(n as usize)))
            } else {
                Err(RuntimeError::FixnumOverflow)
            }
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl Shr for Fixnum {
    type Output = Result<Fixnum, RuntimeError>;

    fn shr(self, rhs: Self) -> Self::Output {
        let shift: isize = rhs.into();

        if 0 <= shift && shift < Self::WIDTH as isize {
            Ok(Self(Value((((self.0).0 as isize) >> shift) as usize & !Value::MASK)))
        } else {
            Err(RuntimeError::FixnumOverflow)
        }
    }
}

impl PartialOrd for Fixnum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { (self.0).0.partial_cmp(&(other.0).0) }

    fn lt(&self, other: &Self) -> bool { (self.0).0.lt(&(other.0).0) }
    fn le(&self, other: &Self) -> bool { (self.0).0.le(&(other.0).0) }
    fn gt(&self, other: &Self) -> bool { (self.0).0.gt(&(other.0).0) }
    fn ge(&self, other: &Self) -> bool { (self.0).0.ge(&(other.0).0) }
}

impl Display for Fixnum {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", <Fixnum as Into<isize>>::into(*self)) // HACK
    }
}

// ---

#[derive(Debug, Clone, Copy, EnumIter)]
#[repr(usize)]
pub enum Primop {
    Void = 0 << Value::SHIFT | Tag::Fixnum as usize,
    Eq = 1 << Value::SHIFT | Tag::Fixnum as usize,
    Type = 2 << Value::SHIFT | Tag::Fixnum as usize,
    IdentityHash = 3 << Value::SHIFT | Tag::Fixnum as usize,
    Call = 4 << Value::SHIFT | Tag::Fixnum as usize,
    Apply = 5 << Value::SHIFT | Tag::Fixnum as usize,
    CallWithValues = 6 << Value::SHIFT | Tag::Fixnum as usize,
    Values = 7 << Value::SHIFT | Tag::Fixnum as usize,
    SymbolHash = 8 << Value::SHIFT | Tag::Fixnum as usize,
    ImmediateTypeIndex = 9 << Value::SHIFT | Tag::Fixnum as usize,
    Length = 10 << Value::SHIFT | Tag::Fixnum as usize,
    SlotRef = 11 << Value::SHIFT | Tag::Fixnum as usize,
    SlotSet = 12 << Value::SHIFT | Tag::Fixnum as usize,
    Record = 13 << Value::SHIFT | Tag::Fixnum as usize,
    Cons = 14 << Value::SHIFT | Tag::Fixnum as usize,
    Car = 15 << Value::SHIFT | Tag::Fixnum as usize,
    Cdr = 16 << Value::SHIFT | Tag::Fixnum as usize,
    MakeVector = 17 << Value::SHIFT | Tag::Fixnum as usize,
    FxLt = 18 << Value::SHIFT | Tag::Fixnum as usize,
    FxAdd = 19 << Value::SHIFT | Tag::Fixnum as usize,
    FxSub = 20 << Value::SHIFT | Tag::Fixnum as usize,
    FxMul = 21 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseAnd = 24 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseIor = 25 << Value::SHIFT | Tag::Fixnum as usize,
    BitwiseXor = 26 << Value::SHIFT | Tag::Fixnum as usize,
    FxArithmeticShift = 27 << Value::SHIFT | Tag::Fixnum as usize,
    BitCount = 28 << Value::SHIFT | Tag::Fixnum as usize,
    MakeSyntax = 29 << Value::SHIFT | Tag::Fixnum as usize,
    MakeType = 30 << Value::SHIFT | Tag::Fixnum as usize
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
            Length => write!(f, "object-length"),
            SlotRef => write!(f, "slot-ref"),
            SlotSet => write!(f, "slot-set!"),
            Record => write!(f, "record"),
            Cons => write!(f, "cons"),
            Car => write!(f, "car"),
            Cdr => write!(f, "cdr"),
            MakeVector => write!(f, "make-vector"),
            FxLt => write!(f, "fx<?"),
            FxAdd => write!(f, "fx+"),
            FxSub => write!(f, "fx-"),
            FxMul => write!(f, "fx*"),
            BitwiseAnd => write!(f, "bitwise-and"),
            BitwiseIor => write!(f, "bitwise-ior"),
            BitwiseXor => write!(f, "bitwise-xor"),
            FxArithmeticShift => write!(f, "arithmetic-shift"),
            BitCount => write!(f, "bit-count"),
            MakeSyntax => write!(f, "make-syntax"),
            MakeType => write!(f, "make-type"),
            Type => write!(f, "type")
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
    pub fn header(self) -> *const Object { unsafe { (self.data() as *const Object).offset(-1) } }

    pub fn header_mut(self) -> *mut Object { unsafe { (self.data() as *mut Object).offset(-1) } }

    pub fn data(self) -> *mut u8 { (self.value.0 - Tag::ORef as usize) as *mut u8 }

    pub fn slots<'a>(&'a self) -> &'a [Value] {
        unsafe {
            let obj = &mut *self.header_mut();
            slice::from_raw_parts(
                obj.data() as *mut Value,
                if obj.is_bytes() { 0 } else { obj.len() }
            )
        }
    }

    pub fn slots_mut<'a>(&'a mut self) -> &'a mut [Value] {
        unsafe {
            let obj = &mut *self.header_mut();
            slice::from_raw_parts_mut(
                obj.data() as *mut Value,
                if obj.is_bytes() { 0 } else { obj.len() }
            )
        }
    }

    pub fn identity_hash(self) -> usize { unsafe { (*self.header_mut()).identity_hash() } }
}

impl HeapValue<()> {
    pub fn typ(self) -> Type { unsafe { (*self.header()).typ() } }

    pub fn unpack(self, state: &State) -> UnpackedHeapValue {
        if let Ok(vec) = state.downcast::<Vector>(self.into()) {
            UnpackedHeapValue::Vector(vec)
        } else if let Ok(s) = state.downcast::<PgsString>(self.into()) {
            UnpackedHeapValue::String(s)
        } else if let Ok(s) = state.downcast::<Symbol>(self.into()) {
            UnpackedHeapValue::Symbol(s)
        } else if let Ok(p) = state.downcast::<Pair>(self.into()) {
            UnpackedHeapValue::Pair(p)
        } else if let Ok(env) = state.downcast::<Bindings>(self.into()) {
            UnpackedHeapValue::Bindings(env)
        } else if let Ok(f) = state.downcast::<Closure>(self.into()) {
            UnpackedHeapValue::Closure(f)
        } else if let Ok(s) = state.downcast::<Syntax>(self.into()) {
            UnpackedHeapValue::Syntax(s)
        } else if let Ok(t) = state.downcast::<Type>(self.into()) {
            UnpackedHeapValue::Type(t)
        } else {
            todo!()
        }
    }
}

impl<T: DynamicType<IsFlex = False>> Deref for HeapValue<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { unsafe { &*(self.data() as *const T) } }
}

impl<T: DynamicType<IsFlex = False>> DerefMut for HeapValue<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { unsafe { &mut *(self.data() as *mut T) } }
}

impl<T: ?Sized> From<HeapValue<T>> for Value {
    fn from(v: HeapValue<T>) -> Self { v.value }
}

impl TryFrom<Value> for HeapValue<()> {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.has_tag(Tag::ORef) {
            Ok(Self { value, _phantom: PhantomData })
        } else {
            Err(RuntimeError::NonObject(value))
        }
    }
}

impl DynamicDowncast for HeapValue<()> {
    fn downcast(_: &State, v: Value) -> Result<Self, RuntimeError> { Self::try_from(v) }

    unsafe fn unchecked_downcast(value: Value) -> Self {
        HeapValue { value, _phantom: PhantomData }
    }
}

impl<T: DynamicType<IsFlex = False> + Debug> Debug for HeapValue<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { <T as Debug>::fmt(&*self, f) }
}

impl Display for HeapValue<()> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { todo!() }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let state = State::new(&[], 1 << 20, 1 << 20);

        let c = 'a';

        let v = Value::from(c);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(c, state.downcast(v).unwrap());
    }

    #[test]
    fn test_bool() {
        let state = State::new(&[], 1 << 20, 1 << 20);

        let b = true;

        let v = Value::from(b);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(b, state.downcast(v).unwrap());
    }

    #[test]
    fn test_fixnum() {
        let state = State::new(&[], 1 << 20, 1 << 20);

        let n = 23isize;

        let v = Value::from(23i16);

        assert!(!v.has_tag(Tag::ORef));
        assert_eq!(n, state.downcast(v).unwrap());

        let m = -n;

        let u = Value::try_from(m).unwrap();

        assert!(!u.has_tag(Tag::ORef));
        assert_eq!(m, state.downcast(u).unwrap());
    }
}

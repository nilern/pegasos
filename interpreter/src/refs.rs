use std::char;
use std::collections::hash_map::DefaultHasher;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, size_of, transmute};
use std::ops::{Deref, DerefMut};
use std::slice;

use super::gc::{HeapObject, ObjectReference};
use super::interpreter::RuntimeError;
use super::objects::{BuiltInType, HeapTag, Heaped, Object, UnpackedHeapValue};
use super::util::fsize;

// ---

#[derive(Debug, PartialEq)]
enum BaseTag {
    ORef = 0b01,
    Fixnum = 0b00, // i30 | i62
    Flonum = 0b10, // f30 | f62
    Ext = 0b11     // char, bool, '() etc.
}

#[derive(Debug, PartialEq)]
pub enum Tag {
    ORef = 0b01,
    Fixnum = 0b00,
    Flonum = 0b10,
    Char = 0b0111, // (28 | 60) bit char
    Bool = 0b1011,
    Singleton = 0b0011, // '() etc.
    FrameTag = 0b1111   // not visible on Scheme side
}

pub enum UnpackedValue {
    ORef(HeapValue<()>),
    Fixnum(isize),
    Flonum(fsize),
    Char(char),
    Bool(bool),
    Nil,
    Unbound,
    Unspecified,
    Eof,
    FrameTag(FrameTag)
}

// ---

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(usize);

impl ObjectReference for Value {
    type Object = Object;

    unsafe fn from_ptr(data: *mut u8) -> Self { Self(data as usize | BaseTag::ORef as usize) }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.is_oref() {
            Some(unsafe { ((self.0 & !Self::MASK) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

impl Value {
    pub const SHIFT: usize = 2;
    const TAG_COUNT: usize = 1 << Self::SHIFT;
    const MASK: usize = Self::TAG_COUNT - 1;

    const EXT_SHIFT: usize = 4;
    const EXT_MASK: usize = (1 << Self::EXT_SHIFT) - 1;

    pub const ZERO: Self = Self(0 << Self::SHIFT | Tag::Fixnum as usize);

    pub const TRUE: Self = Self(1 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const FALSE: Self = Self(0 << Self::EXT_SHIFT | Tag::Bool as usize);
    pub const NIL: Self = Self(0 << Self::EXT_SHIFT | Tag::Singleton as usize); // '()
    pub const UNBOUND: Self = Self(1 << Self::EXT_SHIFT | Tag::Singleton as usize); // 'tombstone'
    pub const UNSPECIFIED: Self = Self(2 << Self::EXT_SHIFT | Tag::Singleton as usize); // for 'unspecified' stuff
    pub const EOF: Self = Self(3 << Self::EXT_SHIFT | Tag::Singleton as usize); // #!eof

    const BOUNDS_SHIFT: usize = 8 * size_of::<Self>() - Self::SHIFT; // 30/62

    fn base_tag(self) -> BaseTag { unsafe { transmute((self.0 & Self::MASK) as u8) } }

    pub fn tag(self) -> Tag {
        let base_tag = self.base_tag();
        if BaseTag::Ext == base_tag {
            unsafe { transmute((self.0 & Self::EXT_MASK) as u8) }
        } else {
            unsafe { transmute(base_tag) }
        }
    }

    pub fn immediate_type_index(self) -> u8 {
        let tag = self.tag();
        if tag == Tag::Singleton {
            ((1 << Self::EXT_SHIFT) | (self.0 >> Self::EXT_SHIFT & Self::EXT_MASK)) as u8
        } else {
            tag as u8
        }
    }

    pub fn is_oref(self) -> bool { self.base_tag() == BaseTag::ORef }

    pub fn is_frame_tag(self) -> bool { self.tag() == Tag::FrameTag }

    pub fn unpack(self) -> UnpackedValue {
        match self.tag() {
            Tag::ORef => UnpackedValue::ORef(HeapValue { value: self, _phantom: PhantomData }),
            Tag::Fixnum => UnpackedValue::Fixnum(self.0 as isize >> Self::SHIFT),
            Tag::Flonum => unimplemented!(),
            Tag::Char => UnpackedValue::Char(unsafe {
                char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32)
            }),
            Tag::Bool => UnpackedValue::Bool((self.0 >> Self::EXT_SHIFT) != 0),
            Tag::Singleton => match self {
                Self::NIL => UnpackedValue::Nil,
                Self::UNBOUND => UnpackedValue::Unbound,
                Self::UNSPECIFIED => UnpackedValue::Unspecified,
                Self::EOF => UnpackedValue::Eof,
                _ => unimplemented!()
            },
            Tag::FrameTag => UnpackedValue::FrameTag(unsafe { transmute::<Value, FrameTag>(self) })
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
    fn from(n: u16) -> Self { Self((n as usize) << Self::SHIFT | BaseTag::Fixnum as usize) }
}

impl From<i16> for Value {
    fn from(n: i16) -> Self { Self((n as usize) << Self::SHIFT | BaseTag::Fixnum as usize) }
}

impl TryFrom<isize> for Value {
    type Error = RuntimeError;

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        if n >> Self::BOUNDS_SHIFT == 0 || n >> Self::BOUNDS_SHIFT == !0 as isize
        // fits in 30/62 bits, OPTIMIZE
        {
            Ok(Self((n << Self::SHIFT) as usize | BaseTag::Fixnum as usize))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Fixnum))
        }
    }
}

impl TryInto<isize> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<isize, Self::Error> {
        if self.base_tag() == BaseTag::Fixnum {
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
            // fits in 30/62 bits, OPTIMIZE
            Ok(Self(n << Self::SHIFT | BaseTag::Fixnum as usize))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Fixnum))
        }
    }
}

impl TryInto<usize> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<usize, Self::Error> {
        if self.base_tag() == BaseTag::Fixnum {
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
            // fits in 30/62 bits
            Ok(Self(
                unsafe { mem::transmute::<_, usize>(n) } << Self::SHIFT | BaseTag::Flonum as usize
            ))
        } else {
            Err(RuntimeError::Overflow(BuiltInType::Flonum))
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::EXT_SHIFT | Tag::Char as usize) }
}

impl TryInto<char> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<char, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Char as usize {
            Ok(unsafe { char::from_u32_unchecked((self.0 >> Self::EXT_SHIFT) as u32) })
        } else {
            Err(RuntimeError::Type { value: self, expected: BuiltInType::Char })
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::EXT_SHIFT | Tag::Bool as usize) }
}

impl TryInto<bool> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<bool, Self::Error> {
        if self.0 & Self::EXT_MASK == Tag::Bool as usize {
            Ok(self.0 >> Self::EXT_SHIFT != 0)
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
            Flonum(n) => unimplemented!(),
            Char(c) => Display::fmt(&c, f), // HACK
            Bool(true) => Display::fmt("#true", f),
            Bool(false) => Display::fmt("#false", f),
            Nil => Display::fmt("()", f),
            Unbound => Display::fmt("#<unbound>", f), // although we should never actually get here
            Unspecified => Display::fmt("#<unspecified>", f),
            _ => unimplemented!()
        }
    }
}

// ---

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum FrameTag {
    Done = 0 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    CondBranch = 1 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    Define = 2 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    Set = 3 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    Let = 4 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    Arg = 5 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    Stmt = 6 << Value::EXT_SHIFT | Tag::FrameTag as usize,
    CallWithValues = 7 << Value::EXT_SHIFT | Tag::FrameTag as usize
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
            // OPTIMIZE:
            slice::from_raw_parts(
                if obj.skips() {
                    (obj.data() as *mut Value).add(1)
                } else {
                    obj.data() as *mut Value
                },
                if obj.is_bytes() {
                    0
                } else if obj.skips() {
                    obj.len() - 1
                } else {
                    obj.len()
                }
            )
        }
    }

    pub fn slots_mut<'a>(&'a mut self) -> &'a mut [Value] {
        unsafe {
            let obj = &mut *self.as_ptr();
            // OPTIMIZE:
            slice::from_raw_parts_mut(
                if obj.skips() {
                    (obj.data() as *mut Value).add(1)
                } else {
                    obj.data() as *mut Value
                },
                if obj.is_bytes() {
                    0
                } else if obj.skips() {
                    obj.len() - 1
                } else {
                    obj.len()
                }
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
        if value.base_tag() == BaseTag::ORef {
            Ok(Self { value, _phantom: PhantomData })
        } else {
            Err(())
        }
    }
}

impl<T: Heaped + ?Sized> TryFrom<Value> for HeapValue<T> {
    type Error = (); // FIXME

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Ok(oref) = HeapValue::<()>::try_from(value) {
            if oref.heap_tag() == T::TAG {
                Ok(HeapValue { value: oref.value, _phantom: PhantomData })
            } else {
                Err(())
            }
        } else {
            Err(())
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
        let n = 23isize;

        let v = Value::from(23i16);

        assert!(!v.is_oref());
        assert_eq!(n, v.try_into().unwrap());

        let m = -n;

        let u = Value::try_from(m).unwrap();

        assert!(!u.is_oref());
        assert_eq!(m, u.try_into().unwrap());
    }
}

use std::convert::TryFrom;
use std::mem::{self, size_of, align_of};

use super::util::fsize;
use super::gc::{ObjectReference, HeapObject};

// ---

#[derive(Clone, Copy)]
struct Value(usize);

impl ObjectReference for Value {
    type Object = Object;

    unsafe fn from_ptr(ptr: *mut Self::Object) -> Self {
        Self((*ptr).data() as usize | Self::OREF_TAG)
    }

    fn as_mut_ptr(self) -> Option<*mut Object> {
        if self.is_oref() {
            Some(unsafe { ((self.0 & !Self::MASK) as *mut Object).offset(-1) })
        } else {
            None
        }
    }
}

impl Value {
    const SHIFT: usize = 2;
    const TAG_COUNT: usize = 1 << Self::SHIFT;
    const MASK: usize = Self::TAG_COUNT - 1;

    const OREF_TAG: usize = 0b01;
    const FIX_TAG: usize = 0b00; // i30 | i62
    const FLO_TAG: usize = 0b10; // f30 | f62
    const EXT_TAG: usize = 0b11; // char, '(), #t, #f etc.

    const EXT_SHIFT: usize = 4;

    const CHAR_TAG: usize = 0b0111; // 28/60 bit char
    const BOOL_TAG: usize = 0b1011; // bool
    // 0b0011 is unused
    const EXT_EXT_TAG: usize = 0b1111; // '(), #!eof etc.

    const NIL: Self = Self(0 & Self::EXT_EXT_TAG); // '()
    const UNDEFINED: Self = Self(1 & Self::EXT_EXT_TAG); // for 'unspecified' stuff
    const EOF: Self = Self(2 & Self::EXT_EXT_TAG); // #!eof

    fn tag(self) -> usize { self.0 & Self::MASK }

    fn is_oref(self) -> bool { self.tag() == Self::OREF_TAG }
}

impl TryFrom<isize> for Value {
    type Error = (); // FIXME

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        if unimplemented!() { // fits in 30/62 bits
            Ok(Self((n as usize) << Self::SHIFT | Self::FIX_TAG))
        } else {
            Err(())
        }
    }
}

impl TryFrom<fsize> for Value {
    type Error = (); // FIXME

    fn try_from(n: fsize) -> Result<Self, Self::Error> {
        if unimplemented!() { // fits in 30/62 bits
            Ok(Self(unsafe { mem::transmute::<_, usize>(n) } << Self::SHIFT | Self::FLO_TAG))
        } else {
            Err(())
        }
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self { Self((c as usize) << Self::EXT_SHIFT | Self::CHAR_TAG) }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self { Self((b as usize) << Self::EXT_SHIFT | Self::BOOL_TAG) }
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
    const SIZE_SHIFT: usize = 8;

    const BYTES_BIT: usize = 0b10;

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

    fn align(&self) -> usize {
        if self.is_bytes() {
            align_of::<u8>()
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
            Some(Value((self.0 & !Self::FWD_MASK) | Value::OREF_TAG))
        } else {
            None
        }
    }
}

// ---

#[derive(Clone, Copy)]
struct Object {
    header: Header
}

impl Object {
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

struct PtrFields {
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


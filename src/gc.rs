use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::mem::{size_of, align_of};

// ---

trait HeapObject {
    type Ref: ObjectReference;
    type Header: ObjectHeader;
}

trait ObjectReference {
    unsafe fn from_ptr(ptr: *mut u8) -> Self;
}

trait ObjectHeader {
    fn size(&self) -> usize;
    fn align(&self) -> usize;
}

// ---

struct Semispace<O: HeapObject> {
    start: *mut u8,
    end: *mut u8,
    phantom: PhantomData<*const O>
}

impl<O: HeapObject> Semispace<O> {
    fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, align_of::<O::Ref>()).unwrap();
        let start = unsafe { alloc::alloc(layout) };
        let end = unsafe { start.add(size) }; // safe since alloc::alloc succeeded
        Self {start, end, phantom: PhantomData}
    }
}

// ---

struct MemoryManager<O: HeapObject> {
    max_heap: usize,
    fromspace: Semispace<O>,
    tospace: Semispace<O>,
    free: *mut u8
}

impl<O: HeapObject> MemoryManager<O> {
    fn new(initial_heap: usize, max_heap: usize) -> Self {
        debug_assert!(initial_heap <= max_heap);
        let fromspace = Semispace::new(initial_heap / 2);
        let tospace = Semispace::new(initial_heap / 2);
        let free = tospace.start;
        Self {max_heap, fromspace, tospace, free}
    }

    fn alloc(&mut self, header: O::Header) -> Option<O::Ref> {
        let mut free = self.free as usize;

        let obj = free.checked_add(align_of::<O::Header>() - 1)? & !(align_of::<O::Header>() - 1);
        let mut data = obj.checked_add(size_of::<O::Header>())?;

        data = data.checked_add(header.align() - 1)? & !(header.align() - 1);
        free = data.checked_add(header.size())?;

        if free <= self.tospace.end as usize {
            self.free = free as *mut u8;
            let header_mem = obj as *mut O::Header;
            unsafe { *header_mem = header };
            Some(unsafe { O::Ref::from_ptr(data as *mut u8) })
        } else {
            None
        }
    }
}

// ---

#[cfg(test)]
mod tests {
    use super::*;

    struct Obj;

    impl HeapObject for Obj {
        type Ref = *mut u8;
        type Header = Hdr;
    }

    struct Hdr {
        size: usize,
        align: usize
    }

    impl ObjectHeader for Hdr {
        fn size(&self) -> usize { self.size }
        fn align(&self) -> usize { self.align }
    }

    impl ObjectReference for *mut u8 {
        unsafe fn from_ptr(ptr: *mut u8) -> Self { ptr }
    }

    #[test]
    fn test_alloc() {
        let header = Hdr {size: 32, align: 8};
        let mut heap: MemoryManager<Obj> = MemoryManager::new(4 << 10, 1 << 20);
        assert!(heap.alloc(header).is_some());
    }
}


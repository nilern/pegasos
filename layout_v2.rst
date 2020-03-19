Heap layout v. 2
================

* Get rid of HeapTag
* Also get rid of the closure fn ptr field by switching to ``##intrinsic``
  syntax instead.
* Heap values are header + sequence of (vrefs | bytes)
* Heap headers have: 1 word identity hash, 1 word type vref
* Types can have one indexed field, but it has to be last => such types must be
  sealed. The indexed field starts with element count and then come the elements.
  The element count is always an aligned fixnum, even in bytes objects.
* Type values start with flags for opaque and sealed. Then they have an
  indexed field with the field specs (mutability and name). The length of that
  indexed field is the minimum length of instances (non-indexed fields +
  indexed field length).


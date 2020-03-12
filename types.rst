Types
=====

* Record types (= ERR5RS record type descriptors)
* Opaque types (most types like list, port etc.)
* Abstract types (e.g. the numeric tower where `integer?` has
  both fixnums and bignums as instances)

We have nominative subtyping with single inheritance.

Type Hierarchy
--------------

`<any>` is the top type. Most opaque or record types are its direct subtypes.
The numeric tower is represented as abstract types that have fixnums etc. as
subtypes. Record field inheritance is also reflected in the subtype hierarchy.

Kind hierarchy
==============

Kinds (or metaclasses, whatever) also form a hierarchy::

       <type>
          |------------------.
          |                  |
    <data-type>       <abstract-type>
          |
    <record-type>
          |
    <extensible-record-type>

(Like any other type, `<type>` is a subtype of `<any>`.)

`<type>` is just an abstract type.

Types of kind `<abstract-type>` don't have direct instances, but can be
inherited from. This is sufficient and necessary for e.g. the numeric tower and
the kind hierarchy itself.

Types of kind `<data-type>` are "concrete", i.e. have direct instances. Types
of kind `<record-type>` are record type descriptors which means they have
public fields (since field accessors can be obtained via reflection even if the
identifiers created by `define-record-type` were not exported). Types of kind
`<extensible-record-type>` can be inherited from, with field inheritance.

Tying the Knot
--------------

`<any>` and `<type>` are `<abstract-type>`:s. The other kinds are
`<data-type>`:s.

Creating New Types
==================

Abstract types can inherit from an abstract type and are given kind
`<abstract-type>`.

Datatypes can inherit from an `<abstract-type>` or an
`<extensible-record-type>`. Opaque datatypes can only inherit from an
`<abstract-type>` and get kind `<data-type>`. Record types get kind
`<record-type>` if they are sealed and otherwise `<extensible-record-type>`.


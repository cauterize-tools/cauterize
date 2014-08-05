# Cauterize

## Alternatives to Cauterize

* Cap'n Proto
* Abstract Syntax Notation One (ASN.1)
* Concise Binary Object Representation (CBOR)
* Apache Thrift
* Apache Avro
* BERT-RPC
* Protocol Buffers
* Message Pack
* Binary JSON (BSON)
* Fressian (Datomic)
* Extensible Binary Meta Language (EBML)
* [extprot](https://github.com/mfp/extprot)
* [FlatBuffers](http://google-opensource.blogspot.com/2014/06/flatbuffers-memory-efficient.html)
* [Simple Binary Encoding (SBE)](http://mechanical-sympathy.blogspot.com/2014/05/simple-binary-encoding.html)
* [Piqi](http://piqi.org/)
* [Transit](https://github.com/cognitect/transit-format)

## Primary Goals

* Unambiguous
* Fixed-upper-bound memory usage
* Fixed-upper-bound execution time
* Idioms native to target languages
* Precise specifications
* Suitable for small systems and networks
* Simplicity
* Early detection of protocol drift
* Structural protocol versioning

## Schemas

### Types

#### BuiltIn

There are several types that represent the foundation of the Cauterize types.
These are the funadmental types available for creating your own more complex
types.

Integral types are assumed to be little endian in their encoded form.

Unsigned integer types:

  * `u8`
  * `u16`
  * `u32`
  * `u64`

Signed values are assumed to be two's complement and little endian.

Signed integer types:

  * `s8`
  * `s16`
  * `s32`
  * `s64`

Booleans are always encoded as a single byte.

A single boolean type:

  * `bool`

Floating point types are hard. Their definitions can be different (or missing
entirely!) across CPU architectures. Therefor, we only define the two most
commonly used floating point representations. They both have unambiguous binary
represntation. The `ieee754s` is a single-precision IEEE 754 single-precision
32 bit floating point value. The `ieee754d` is is a double-precision IEEE 754
double-precision 64-bit floating point value.

Floating point types:

  * `ieee754s`
  * `ieee754d`

New built-in types cannot be defined in a schema.

#### Scalars

Scalars are exactly the same as `BuiltIn` types except that they have a custom
name. In generated code, they are represented logically as if they were the
referenced `BuiltIn` type. They simply create a new name for an existing
BuiltIn type.

Declaring a `Scalar` takes on the following form:

```scheme
(scalar [type name] [built-in type name])
```

#### Constants

Constants are designed to allow the explicit placement of a literal value
inside of a type. These literal values are (currently) limited to being one of
the built-in types. In the future, it may be possible to include
user-defined-type literals in a schema, but not yet.

Constants allow schemas to include explicit values.

The general pattern is as follows:

```scheme
(const [type name] [built-in type] [value])
```

This allows us to express literal values like the following:

```scheme
(const specialNumber u64 4993)
```

When constants are written into the serialization stream, they are represented
as the specified built-in type would have been specified, but with the
prescribed value.

#### Arrays

Arrays are types that allow the expression of a sequence of identical
types where the sequence only ever makes sense with a fixed number of members.
Consider a MAC address (always 6 bytes) or CAN-bus message payload ((almost)
always 8 bytes).

```scheme
(array [type name] [target type] [array length)
```

As an example, consider this definition of a Array describing a MAC
address:

```scheme
(array macAddress u8 6)
```

#### Vector

Vector are types that allow the expression of a sequence of identical
types up to a maximum length. Vector are like FixedArrays in every way
except that their length must be checked in packing and unpacking.

```scheme
(vector [type name] [target type] [maximum array length])
```

Consider this definition for a generic byte buffer with a maximum length of
4096 bytes.

```scheme
(vector byteBuffer4k u8 4096)
```

#### Structures

Structures are like C structures in every way. They allow the description of a
type that has an ordered list of name-type pairs. These allow the expression of
more complex structures that are only meaningful with multiple values.

The description of a Structure follows the following pattern:

```scheme
(struct [type name] [field list])
```

This allows us to define types like the following `user` type:

```scheme
(struct user (fields
               (field name string64)
               (field yearOfBirth u16)
               (field monthOfBirth u8)
               (field dayOfBirth u8)))
```

#### Enumerations

Enumerations are more akin to Enumerations one might find in Haskell or Rust.
That is, they are able to express a possible value in a type, but they are also
able to express values that can contain another value. Each instance of an
enumeration type can only ever contain one of its possible value variants at
any given time.

Enumerations, in C, are represented as tagged unions. That is, a struct that
associates a type tag together with a union type. The type tag needs to be
checked at runtime to determine which type variant is currently instantiated.

```scheme
(enum [type name] [field list])
```
Consider the following example for a 'request message' to some key-value
storage service:

```scheme
(enum request (fields
                (field getKeyCount)
                (field checkKeyExists keyName)
                (field getKey keyName)
                (field eraseKey keyName)
                (field setKey keyValuePair)))
```

Noe that the `getKeyCount` variant does not contain any assocaited data while
the `getKey` variant specifies a type that encodes the name of the key to get.

#### Sets

Sets are a closed collection of key-type pairs. The difference between Sets and
Structures is that the Set can, at any given time, only include *some* of its
possible members. The presense of encoded members is stored as a bitfield
encoded before any contained data.

```scheme
(set [type name] [field list])
```
As an example, consider the following description of a type capable of storing
changes in some observed values in a sensor rig:

```scheme
(set sensed (fields
              (field ambientTemp u16)
              (field ambientLight u16)
              (field airPressure u16)
              (field positionX u32)
              (field positionY u32)
              (field positionZ u32)))
```

If no sensor difference is detected in a specific sensor in a given time slice,
the value won't be included in the serialized value. This allows users to
encode values that only have deltas in a space-efficient way.

Sets can also be used as bitfields. Consider the following set definition that
can be used to indicate which lights on a car are currently powered/lit:

```scheme
(set poweredLights (fields
                     (field headlights)
                     (field taillights)
                     (field leftblinkers)
                     (field rightblinkers)
                     (field breaklights)))
```

This set defines 5 fields, but none of the fields have associated data. This
allows cauterize to express the entirey of this structure in a single word at
least 5 bits wide.

#### Padding

Padding types can be used to insert null bytes into a payload. Padding types
must be 0 in the stream. Any other value will result in a pack/unpack error.

```scheme
(pad [type name] [padding width in bytes])
```

The following defines a type that can only be represented by 8 null bytes.

```scheme
(pad p8 8)
```

### Field Lists

Field lists are used to designate a set of (name/type) pairs. These pairs are
used in any of the types that are created out of other types. This includes:
structs, enums, sets, and partials.

They are defined like this:

```scheme
(fields
  (field [field name] [optional target type])
  (field ...))
```

If the target type is not specified in the schema, code generators will only
define a field index and will not attempt to associate the field with a
contained type.

## Specifications

TBD

# TODO List

* Exhaustive checking for hash-collisions.
* Hash algorithm selection.
* Prune unused builtins out of specifications.
* Note that specifications must list their dependencies in topographical order.
* Add a "ranged" type that's similar to a scalar but only accepts n..m values in a builtin.
* Prove hashes are on the expected content.
* Ensure that const values will fit inside the specified builtin type.

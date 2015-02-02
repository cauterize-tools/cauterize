# Cauterize

Cauterize is a _schema language_ for describing ordinary data and a compiler to
translate a schema into an unambiguous intermediate representation known as a
_specification_.

This specification describes all inferrable information from the schema to make
the creation of _code generators_ easier. These generators consume the
specification and output a library capable of reading and writing data
represented by the schema.

Cauterize is _first_ intended to serve the constraints systems and embedded
systems programming, but is still suitable for a wide variety of other
situations. All Cauterize specifications have the following properties:

  * Encoded messages have a maximum and minimum size known at compile time
  * All types have a maximum referential depth
  * All types are neither directly nor indirectly recursive
  * All types are listed in a topographically sorted order (all types that
    depend on other types will follow those types in the specification output)
  * The specification has a version-hash that is based on all possible
    variation in the schema. This can be used to detect incidental (as opposed
    to adversarial) protocol drift with a very high probability.
  * All types have a type-hash based on the structure of that type and all
    types it depends on
  * All length, field, and type tags have their representation optimized to use
    as little space as possible while still maintaining a minimum alignment of
    8 bits

Cauterize was first designed to work on a small embedded system that required
all memory to be allocated ahead of time (no dynamic memory allocation) yet
still needed to interface with a Ruby and C# environment. The prototype was a
Ruby DSL that, when executed, would emit C, C#, and Ruby libraries that were
all capable of reading and writing the schema described by the DSL.

This project is a successor to the original Ruby DSL prototype with a goals of
being safer, being more complete, and including features making it easier to
add new code generators.

In order to better frame the context at which Cauterize is targeted, here's a
incomplete list of other tools that attempt to perform some or all of the
functions Cauterize is capable of performing. If Cauterize is not right for
your purposes, perhaps one of these tools is. These are listed alphabetically.

* [Abstract Syntax Notation One (ASN.1)](http://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One)
* [Apache Avro](http://avro.apache.org/docs/current/)
* [Apache Thrift](https://thrift.apache.org/)
* [BERT-RPC](http://bert-rpc.org/)
* [Binary JSON (BSON)](http://bsonspec.org/)
* [Cap'n Proto](https://capnproto.org/)
* [Concise Binary Object Representation (CBOR)](http://cbor.io/)
* [Extensible Binary Meta Language (EBML)](http://ebml.sourceforge.net/)
* [FlatBuffers](http://google-opensource.blogspot.com/2014/06/flatbuffers-memory-efficient.html)
* [Fressian (Datomic)](https://github.com/Datomic/fressian)
* [Message Pack](http://msgpack.org/)
* [Piqi](http://piqi.org/)
* [Protocol Buffers](https://developers.google.com/protocol-buffers/)
* [Simple Binary Encoding (SBE)](http://mechanical-sympathy.blogspot.com/2014/05/simple-binary-encoding.html)
* [Transit](https://github.com/cognitect/transit-format)
* [XDR](http://en.wikipedia.org/wiki/External_Data_Representation)
* [bond](https://github.com/Microsoft/bond)
* [extprot](https://github.com/mfp/extprot)

## Goals

Cauterize has a single goal that informs all its other goals: Cauterize must
always be suitable to target at a hard-real time embedded system without
dynamic memory allocation.

## Primary Goals

From this goal, we can extract the following more specific goals:

* Must be achievable with static memory allocation - not all embedded systems
  support dynamic allocation.
* Must be achievable in bounded execution time - hard-real-time systems must
  know how long each operation can possibly take.
* Must have methods for detecting protocol drift early - embedded systems are
  often harder to update than desktop systems. They have longer deployment in
  more unusual conditions. Therefor, it is very important that the version of
  the messages being used by the embedded systems is detectable by its partner
  systems and that they be kept in sync.
* Specifications must be precise in as many ways as possible - many embedded
  systems vary from standard desktop and server systems in unusual ways. These
  variations can can include things such as: the number of bits in a byte, the
  amount of memory available on the system, the representation of pointers, the
  endianness of the processor, and the format of floating point numbers.
* Should not preclude other systems - though embedded systems are a primary
  target, design choices for Cauterize should not preclude the use of Cauterize
  on systems such as mobile, desktop, and web development.

## Secondary Goals

* Ease of implementation - code generators should be able to represent the
  specification in idioms common in the target language. In C, this is structs,
  enumerations, and unions. In Ruby, this would likely be classes.
* Simplicity - code generators should not be expected to perform complicated
  operations in order to emit code. Concepts should be simple in nature and
  have at least one obvious method for implementation.

## Schemas

### Types

#### BuiltIn

There are several types that represent the foundation of the Cauterize types.
These are the fundamental types available for creating your own more complex
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
representation. The `ieee754s` is a single-precision IEEE 754 single-precision
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

Noe that the `getKeyCount` variant does not contain any associated data while
the `getKey` variant specifies a type that encodes the name of the key to get.

#### Sets

Sets are a closed collection of key-type pairs. The difference between Sets and
Structures is that the Set can, at any given time, only include *some* of its
possible members. The presence of encoded members is stored as a bitfield
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
allows cauterize to express the entirety of this structure in a single word at
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
* Add a "ranged" type that's similar to a scalar but only accepts n..m values in a builtin.
* Prove hashes are on the expected content.

## Generic Interface

The generic interface is designed to allow outside infrastructure, agnostic to
the Cauterize payload, to reliably check and move the data.

### Agnostic Interface Format

```
Size        Field
=====================================
1           'c' (check/seek byte)
1           'a' (check/seek byte)
1           Agnostic Interface Version
1           Type/Data Length (Bits 0 to 4: type tag. Bits 5 to 8: additional length bytes.)
size(hash)  Specification Tag
(1-32)      Type Tag
(1-8)       Data Length
length      Data
1           'u' (check/seek byte)
1           't' (check/seek byte)
size(check) Checksum of entire message
```

#### Descriptions

```
'c' (check/seek byte)
'a' (check/seek byte)
```
Each message in the AI format starts with two bytes: 'c', and 'a'. These bytes
can be used to indicate the start of a message. These bytes may be common bytes
in the stream, but there should be enough information in the remainder of the
AI message to validate the message.

```
Agnostic Interface Version
```

After the two check bytes, there's a version indicator: this allows the AI
format to contain additional variation in the future. This document currently
only defines version 0 of the AI specification. 

```
Type/Data Length (Bits 0 to 4: type tag. Bits 5 to 8: additional length bytes.)
```

After the AI version is a byte that indicates two values: the number of
additional bytes (greater than 1) to use for the type tag and the number of
additional bytes (greater than 1) to use for the data length.

The first 5 bites (bits 0 to 4) encode the type tag. The last 3 bits (bits 5 to
8) encode the length bytes. This gives us a maximum type tag length of 32 bytes
and a maximum length field of 8 bytes.

Note: sha1 is 20 bytes.
Note: 2^(8*8) is a lot of bytes.

```
Specification Tag
```

After the length fields comes the specification tag. The specification tag is
always printed in full. For a SHA1, this is 20 bytes.

```
Type Tag
```

The type tag is built by taking a prefix of the type hash this message encodes.
The length of the prefix is defined by the value in the Type/Data Length field.

Note: this field must be long enough to ensure an unambiguous prefix-to-type
map.

```
Data Length
```

The data length field encodes how much data is used to encode the message data.
Reading this length from the stream will read out the data used to encode the cauterize type.

```
Data
```

This is the encoded catuerize data.


```
'u' (check/seek byte)
't' (check/seek byte)
```

Two more seek bytes help quickly validate the message.

```
Checksum of entire message
```

Finally, a CRC/hash of the entire message up to, but not including, the
CRC/hash is appended to the end. If the calculated hash matches this value, the
message is almost certainly valid (excepting a motivated attacker).

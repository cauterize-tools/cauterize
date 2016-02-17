```
   )     _____             _            _
  /((   /  __ \           | |          (_)
 (_))\  | /  \/ __ _ _   _| |_ ___ _ __ _ _______
 _)((_) | |    / _` | | | | __/ _ \ '__| |_  / _ \
 \ ^ /  | \__/\ (_| | |_| | ||  __/ |  | |/ /  __/
  \_/    \____/\__,_|\__,_|\__\___|_|  |_/___\___|
```

# Cauterize

Cauterize is a data-description language suitable for hard-real-time systems
and systems without dynamic memory allocation. It can be used instead of other
data description languages like JSON, XML, or ProtocolBuffers.

## Introduction

Cauterize is _first_ intended to serve the constraints of [hard
real-time](http://en.wikipedia.org/wiki/Real-time_computing) embedded systems
programming, but is still suitable for a variety of other situations. All
Cauterize specifications have the following properties:

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
add new code generators beyond the original C, C# and Ruby generators.

In order to better frame the context at which Cauterize is targeted, here's a
incomplete list of other tools that attempt to perform some or all of the
functions Cauterize is capable of performing. If Cauterize is not right for
your purposes, perhaps one of these tools is. These are listed alphabetically.

* [Abstract Syntax Notation One (ASN.1)](http://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One)
* [Apache Avro](http://avro.apache.org/docs/current/)
* [Apache Thrift](https://thrift.apache.org/)
* [BERT-RPC](http://bert-rpc.org/)
* [Binary JSON (BSON)](http://bsonspec.org/)
* [bond](https://github.com/Microsoft/bond)
* [Cap'n Proto](https://capnproto.org/)
* [Concise Binary Object Representation (CBOR)](http://cbor.io/)
* [Extensible Binary Meta Language (EBML)](http://ebml.sourceforge.net/)
* [extprot](https://github.com/mfp/extprot)
* [FlatBuffers](http://google-opensource.blogspot.com/2014/06/flatbuffers-memory-efficient.html)
* [Fressian (Datomic)](https://github.com/Datomic/fressian)
* [Linden Labs Structured Data](http://wiki.secondlife.com/wiki/LLSD)
* [Message Pack](http://msgpack.org/)
* [Piqi](http://piqi.org/)
* [Protocol Buffers](https://developers.google.com/protocol-buffers/)
* [Simple Binary Encoding (SBE)](http://mechanical-sympathy.blogspot.com/2014/05/simple-binary-encoding.html)
* [Transit](https://github.com/cognitect/transit-format)
* [XDR](http://en.wikipedia.org/wiki/External_Data_Representation)

### Schema, Specification, and Code Generation

Cauterize consists of several parts: a _schema language_ for describing
ordinary data, a compiler to translate a schema into an intermediate
representation known as a _specification_, and code generators that translate
specifications into encoders and decoders.

A schema is written by humans and it describes the semantic meaning in all
types.

The specification is created by the cauterize compiler and it describes all
inferable information from the schema in order to make the creation of _code
generators_ easier.

Code generators consume the specification and output a library capable of
encoding and decoding data represented by the schema.


### Goals

Cauterize should be suitable for hard-real-time systems without dynamic memory
allocation. From this goal, we can extract the following more specific goals:

* Must be achievable with static memory allocation - not all embedded systems
  support dynamic allocation.
* Must be achievable in bounded execution time - hard-real-time systems must
  know how long each operation can possibly take.
* Must support methods for detecting protocol drift early - embedded systems
  are often harder to update than desktop systems. They have longer deployment
  in more unusual conditions. Therefore, it is very important that the version
  of the messages being used by the embedded systems is detectable by its
  partner systems and that they be kept in sync.
* Specifications must be precise in as many ways as possible - many embedded
  systems vary from standard desktop and server systems in unusual ways. These
  variations can can include things such as: the number of bits in a byte, the
  amount of memory available on the system, the representation of pointers, the
  endianness of the processor, and the format of floating point numbers.

After dealing with these points that enable Cauterize to be used in constrained
systems, there are several other goals that make the quality of life for users
better.

* Should not preclude other systems - though embedded systems are a primary
  target, design choices for Cauterize should not preclude the use of Cauterize
  on systems such as mobile, desktop, and web development.
* Ease of implementation - code generators should be able to represent the
  specification in idioms common in the target language. In C, this is structs,
  enumerations, and unions. In Ruby, this would likely be classes. Furthermore,
  code generators should not have to generate any overly-complicated structures
  to conform to a specification. When a Cauterize feature is proposed, it must
  be implementable in simple terms in a variety of languages.
* Ease of verification - code generators are hard to validate for correctness.
  There should be some means of checking them automatically.
* Simplicity - code generators should not be expected to perform complicated
  operations in order to emit code. Concepts should be simple in nature and
  have at least one obvious method for implementation.

## Schema Language

The schema language uses parentheses to enclose each of its expressions. All
expressions have an assigned order for all arguments.

### Schema Name

The schema name is optional. If it's omitted, it will be set to "schema".

```
(name [name])
```

Names can consist of the following characters and must be enclosed in
double quotations:

`[a-z]([a-z]|[0-9]|_)*`

### Schema Version

The schema version is optional. If it's omittied, it will be set to
"0.0.0". The version may consist of the following characters in
quotations:

```
([a-z]|[0-9])([a-z]|[0-9]|_|.|-)*
```

The reason the name and version patterns are restrictive is to ease the
burden on code generators. Some languages, such as Haskell and Ruby, have
specific rules about capitalization. Without the restrictive name pattern
Cauterize uses, code generators would have to do a lot more work to emit code
that is readable and matches the target language's normal conventions.

### Comments

Line comments are defined by using two `;` characters in a row. Here's an example.

```
;; this is comment
(name "some name")
(version "0.0.1")
```

### Primitive Types

There are several types that represent the foundation of the Cauterize types.
These are the fundamental types available for creating your own more complex
types. It is not possible to define new built-in types in a schema. All
builtins referenced by a schema will have definitions in the output
specification.

#### Unsigned Primitive Types

Unsigned values are encoded as [little
endian](http://en.wikipedia.org/wiki/Endianness).

  * `u8` - 8 bits wide
  * `u16` - 16 bits wide
  * `u32` - 32 bits wide
  * `u64` - 64 bits wide

#### Signed Primitive Types

Signed values are encoded as [two's
complement](http://en.wikipedia.org/wiki/Two%27s_complement) [little
endian](http://en.wikipedia.org/wiki/Endianness) values.

  * `s8` - 8 bits wide
  * `s16` - 16 bits wide
  * `s32` - 32 bits wide
  * `s64` - 64 bits wide

#### Boolean Primitive Type

Booleans are encoded with a single byte. The only valid values are 0 and 1
where 0 represents `false` and 1 represents `true`.

  * `bool` - 8 bits wide

#### Floating Point Primitive Types

Floating point types are hard. Their definitions can be different (or missing
entirely!) across CPU architectures. Therefore, Cauterize only defines the
`f32`  and `f64` types. These are the [IEEE 754 single and double
precision floating point
values](http://en.wikipedia.org/wiki/IEEE_floating_point). The single precision
value uses 32 bits while the double-precision value uses 64 bits. Both flavors
are encoded in [little endian](http://en.wikipedia.org/wiki/Endianness).

Floating point types:

  * `f32` - 32 bits wide, IEEE754 Single Precision
  * `f64` - 64 bits wide, IEEE754 Double Precision

### Prototypes

Cauterize provides several prototypes that act as templates out of which other
types can be created.

All types must list a name. That name follows the following rule:

```
[a-z]([a-z]|[0-9]|_)*
```

#### Synonyms

Synonyms are used to give one of the built-in types a new name. Their encoded
representation is identical to that of the built-in value they wrap, but has
a type that is distinct from the wrapped value.

```
(type [type name] synonym [built-in type name])
```

The following example defines the type `age` that has the same representation
as a `u8`.

```
(type age synonym u8)
```

#### Ranges

Ranges are used to encode an integer value between two other integer
values. They are encoded in a word suitable for expressing all
possible values in the range. That is, a range with less than 256
members will be encoded in a 8 bit word, a range with less than 65535
members will be encoded in a 16 bit word, and so on.

```
(type [type name] range [minimum value] [maximum value])
```

The following is an example of a range that only encodes the values
from 1000 to 1010.

```
(type some_range range 1000 1010)
```

#### Arrays

Arrays are fixed-length sequences of identially typed objects. These are to be
used when the sequence only makes sense with a fixed number of elements.

```
(type [type name] array [element type] [array length])
```

Consider the following example that defines a type `mac` that encodes a Media
Access Control address (which is always 6 bytes long).

```
(type mac array u8 6)
```

#### Vector

Vectors are variable-lengthed sequences of identically typed objects. These are
to be used when a sequence of elements has a maximum length, but may contain
fewer elements.

```
(type [type name] vector [target type] [maximum array length])
```

The following example defines a generic byte buffer with a maximum length of
4096 bytes.

```
(type byte_buffer_4k vector u8 4096)
```

#### Enumeration

Enumerations are types with a fixed set of named members. Members of
an enumeration are assign integer values starting from 0. These values
are assigned automatically by the Cauterize compiler. Enumerations are
encoded in the smallest word necessary to express every value in the
enumeration.

```
(type [type name] enumeration (values [space-separated list of identifiers])
```

The following is an enumeration encoding the days of the week:

```
(type days_of_week enumeration
 (values
  sunday
  monday
  tuesday
  wednesday
  thursday
  friday
  saturday))
```

#### Field Lists

Field lists cannot be defined on their own; they can only be used as the last
parameter to a `record`, `union`, or `combination` expression (which are
defined later in this document). Field lists are used to designate a set of
(name/type) pairs.

Unions and combinations can use the `empty` keyword instead of the
`field` keyword. Empty fields do not have any associated data.

Field lists are defined like this:

```
(fields
  (field [field name] [type]) ;; a field with some data
  (empty [field name])        ;; an empty field (just the tag)
  (field ...))
```

A type is not required. The behavior of a type-less field is dependent on the
enclosing expression.

#### Records

Records are a collection of named fields where each field has a distinct type.

```
(type [type name] record [field list])
```

An empty field in a record lacks any semantic meaning. It can neither be
encoded or represented by code generators.

This is an example of a record describing a person's age, height, and whether or
not they like cats:

```
(type person record (fields (field age u8)
                            (field height u8)
                            (field likes_cats bool)))
```

#### Union

Unions encode a set of possible values and some associated sub-value.  Like
records, their schema entries specify a list of fields, but, unlike records,
only one of those fields can represented in a union at any given time.

Unions in Cauterize are very similar to algebraic data types found in other
languages such as OCaml, Haskell, and Rust.

```
(type [type name] union [field list])
```

An empty field in a union represents that a variant of the union is set. This
has meaning even if there is no associated data. A union where all fields lack
associated data behaves similarly to a C enumeration.

This example shows a `request` type for some key-value storage system. The
system stores `u64` values according to names that are up to 128 bytes long.

```
(type key_name vector key_name u8 128)
(type key_pair record (fields
                       (field name key_name)
                       (field value u64)))

(type request union (fields
                     (field get_key_count)
                     (field check_key_exists key_name)
                     (field get_key key_name)
                     (field erase_key key_name)
                     (field set_key key_pair))))
```

The `get_key_count` variant does not contain any associated data while the
`get_key` variant specifies a type that encodes the name of the key to get.
Note: the `response` type is not defined in this example.

#### Combination

Combinations, like records, are a collection of named fields where each field
has a distinct type. The difference is that each field in the combination can
either be present or not present in the encoded output.

```
(type [type name] combination [field list])
```
As an example, consider the following description of a type capable of storing
changes in some observed values in a sensor rig:

An empty field in a Combination behaves like a boolean flag.

```
(type sensed combination (fields
                          (field ambient_temp u16)
                          (field ambient_light u16)
                          (field air_pressure u16)
                          (field position_x u32)
                          (field position_y u32)
                          (field position_z u32)))
```

If a sensor value hasn't changed since the last time the message was sent, the
message is able to omit that reading since there isn't new information to
share.

## Specification Language

The specification language also uses parenthesis to enclose each of its
expressions. All expressions have an assigned order for all their arguments.
Each expression explains one type defined by the schema.

### Specification-specific Expressions

There are several expressions that show up in specifications that do not show
up in schemas. These expressions represent data inferred from the schema.

#### `sha1` Expression

The `sha1` expression represents a SHA1 hash. It is 40 hexadecimal characters
long.

##### Example

```
(sha1 77e8f0d33bd09411bbc2f94c839e0ccc34d55603)
```

#### `depth` Expression

The `depth` expression represents the maximum referential depth of a schema.
This expression is used in the top level `specification` expression.

##### Example

```
(depth 6)
```

#### `type-width` Expression

The `type-width` expression represents the minimum length of the prefix of each
type hash needed for a unique value. It is only used in the `specification`
expression.

##### Example

The two hashes below have the first two bytes in common. The `type-width` would
be 3 because a 3-byte prefix of each hash is unique.

```
(sha1 77e8f0d33bd09411bbc2f94c839e0ccc34d55603)
(sha1 77e878098602c275eb7a3408aff17e396220324d)
```

The `type-width` expression would show up in the specification as:

```
(type-width 3)
```

#### `length-width` Expression

The `length-width` expression represents the number of bytes suitable for
representing the maximum encoded length of any type in the schema.

This width is always one of 1, 2, 4, or 8.

##### Example

For a schema with a maximum length of 68: `(length-width 1)`.

For a schema with a maximum length of 257: `(length-width 2)`.

For a schema with a maximum length of 70,000: `(length-width 4)`.

For a schema with a maximum length of 17,000,000: `(length-width 4)`.

For a schema with a maximum length of 8,600,000,000: `(length-width 8)`.

#### `fixed-size` Expression

The `fixed-size` expression represents a size in bytes. This expression is used
in type specifications that have a fixed encoding size.

##### Example

```
(fixed-size 8)
```

#### `range-size` Expression

The `range-size` expression represents a minimum and maximum size in bytes.
This expression is used in type specifications that have a variable encoding
size.

##### Example

```
(range-size 22 16406)
```

#### `length-repr` Expression

The `length-repr` expression represents the type used to encode a vector's
length. It only occurs in `vector` specifications.

##### Example

```
(length-repr u8)

```

#### `flags-repr` Expression

The `flags-repr` expression represents the type used to encode a combination's
flags. It only occurs in `combination` specifications.

##### Example

```
(flags-repr u16)
```

#### `tag-repr` Expression

The `tag-repr` expression represents the type used to encode a union's type
tag. It only occurs in `union` specifications.

### `specification` Expression

All specification documents contain several top-level expressions.

```
(name [schema name])
(version [schema version])
(sha1 [a sha1 hash])
(range-size [minimum encoded size] [maximum encoded size])
(depth [maximum referential depth of the schema])
(type-width [type-tag width hint])
(length-width [length-tag width hint])

[[type expressions]]
```

### `synonym` Specification Expression

All synonym expressions have the following layout:

```
(type [type name] synonym (sha1 ...) (fixed-size ...))
```

### `array` Specification Expression

All array expressions have the following layout:

```
(type [type name] array (sha1 ...) (range-size ...)
  [array length] [element type])
```

### `vector` Specification Expression

All vectors have the following layout:

```
(type [type name] vector (sha1 ...) (range-size ...)
  (length-repr ...)
  [vector max length] [element type])
```

### `fields` and `field` Specification Expressions

Record, union, and combination types all include a list of fields. This list of
fields is enclosed in a `fields` expression.

The `fields` expression has this form:

```
(fields [[field expression]])
```

`field` expressions come in two varieties. The first variety expresses a name
for the field, a type the field references, and the index of the field.

```
(field [field name] [type name] [index])
```

The second variety expresses only a name for the field and the index of the
field. These are known as "empty" fields.

```
(field [field name] [index])
```

### `record` Specification Expression

All records have the following layout:

```
(type [type name] record (sha1 ...) (range-size ...)
  (fields ...))
```

### `union` Specification Expression

All unions have the following layout:

```
(type [type name] record (sha1 ...) (range-size ...)
  (tag-repr [built-in type])
  (fields ...))
```

### `combination` Specification Expression

All combinations have the following layout:

```
(type [type name] combination (sha1 ...) (range-size ...)
  (flags-repr [built-in type])
  (fields ...))
```

# Binary Interpretation

It's possible, given a binary encoding of a Cauterize type and the
specification document for the type's schema, to interpret the encoded bytes
into the original structure.

Types have two general flavors: types with a decision to make and types with
only a single interpretation path. The types that have interpretation decisions
are: `vector`, `union`, and `combination`. All others (`builtin`, `synonym`,
`array`, and `record`) only have a single path of interpretation.

Decision types all encode their decision variable in the binary stream. For
`vector` this is a length tag. For `union`, this is a type tag. For
`combination`, the tag encodes a series of flags representing which fields in
the combination are present.

Tag information for types *always* comes before the type data itself. For
example, a vector with a `u8` tag for its length will encode that `u8` as the
very first byte.

Furthermore, the only types that actually contain data are the builtin types.
All other types are constructed from builtin types or other user-defined types.
With knowledge about tag position and where data is stored, we can begin to
parse encoded binary strings.

## Sample Schema

We'll use the following as our schema. There's nothing particularly interesting
about it except that it uses all of the available prototypes. I've called this
file `binterp.caut`.

```
(schema binterp 0.0.0.0
  (synonym syn_u32 u32)

  (array arr_u32 u32 4)

  (vector vec_u32 u32 4)

  (record rec_unsigned
    (fields
      (field fu8 u8)
      (field fu16 u16)
      (field fu32 u32)
      (field fu64 u64)))

  (union union_unsigned
    (fields
      (field fu8 u8)
      (field fu16 u16)
      (field fu32 u32)
      (field fu64 u64)))

  (combination comb_unsigned
    (fields
      (field fu8 u8)
      (field fu16 u16)
      (field fu32 u32)
      (field fu64 u64))))
```

The following command converts this schema into a specification: `cauterize
--schema=binterp.caut --output=binterp.cautspec`.

When we inspect the specification, we see the following:

```
(name "binterp")
(version "0.0.0.0")
(sha1 ac9fda94cadb44d18af85d498f169609fd716efb)
(range-size 1 17) (depth 2) (type-width 1) (length-width 1)
(type vec_u32 vector
  (sha1 35832f3b7bd6dbeb8d3b5c92f73b2f06759d2e7a)
  (range-size 1 17)
  (length-repr u8)
  4 u32)
(type union_unsigned union
  (sha1 e59761d5c25294927e5026c278db565f7190b693)
  (range-size 2 9)
  (tag-repr u8)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
(type syn_u32 synonym
  (sha1 f180b823f00f965e1f0f68ba5c82400f2d9dd32a)
  (fixed-size 4)
  u32)
(type rec_unsigned record
  (sha1 b58dd55deef9faf22ac07ced17cf6f87d1c95111)
  (range-size 15 15)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
(type comb_unsigned combination
  (sha1 d67b5d0a49e122140f418c12ad445ed013a52fc3)
  (range-size 1 16)
  (flags-repr u8)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
(type arr_u32 array
  (sha1 965f3610970341adb1132d27a668a4c94e9e3d57)
  (range-size 16 16)
  4 u32))
```

Using this document and the knowledge of what type we're trying to decode, we
can decode any encoded message for a particular specification.

It's important to remember that an encoded type, on its own, does not contain
enough information to identify it as that type. Two peers wanting to transcode
the same binary stream must agree on what type is being exchanged ahead of time
*or* have a method for identifying which type is being encoded on the wire.
Cauterize generators should normally emit the standard _message interface_
based off the `type-width` and `length-width` parameters in the specification.

In the following exercises, all encoded messages will be listed in hexadecimal.

## Decoding a Primtive

The following is an encoded `u64` type:

```
2a75030000000000
```

Decoding primitives is pretty simple. Each builtin type has a fixed
size. To decode a primitive, read that many bytes from the encoded
string as a little endian value of the proper type.

The above example is, therefore, the following 64-bit value: `0x000000000003752A`.

## Decoding an Array

Decoding arrays is more complex than decoding builtins, but not much more.
Array types all have a specific length. When decoding an array, one has to look
at the array's element type and decode as many of that type as the array's
length expression specifies.

The following is an encoded `arr_u32` type.

```
8c0f0000a30a0000d30d00002c080000
```

Let's take a look at the `arr_u32` specification:

```
(type arr_u32 array
  (sha1 965f3610970341adb1132d27a668a4c94e9e3d57)
  (range-size 16 16)
  4 u32))
```

We know, from the specification, that an `arr_u32` type has a length of 4 and
its element type is `u32`. We know, from its specification, that each `u32` is
made up of 4 bytes. So, all we need to do is read 4 `u32` types from the binary
string.

Therefore, we know that our decoded array is the folling list of `u32` values:

```
[ 0x00000F8C, 0x00000AA3, 0x00000DD3, 0x0000082C ]
```

## Decoding a Record

Decoding a record is quite similar to decoding an array. Both types have a
fixed number of types to decode. The major difference is that arrays decode a
specific number of the same types while records decode a specific number of
varrying types.

The following is an encoded `rec_unsigned` type.

```
fb5e0f0b080000ce85000000000000
```

Let's take a look at the `rec_unsigned` specification:

```
(type rec_unsigned record
  (sha1 b58dd55deef9faf22ac07ced17cf6f87d1c95111)
  (range-size 15 15)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
```

To decode a record, we only need to decode each of the record's fields in order
until we've decoded all the fields. For `rec_unsigned`, this means that we
start by decodin `fu8` and finish by decoding `fu64`.

So, we end up with the following list of decoded values:

```
[ 0xFB, 0x0F5E, 0x0000080B, 0x00000000000085CE ]
```

## Decoding a Vector

Decoding a vector is very similar to decoding an array. The only difference is
that the number of elements to decode is goverend by a length tag rather than
by the type. Vectors define a maximum number of elements to decode rather than
a constant number of elements to decode.

The following is an example of an encoded `vec_u32`.

```
02f8050000aa030000
```

Let's take a look at the specification for a `vec_u32` again.

```
(type vec_u32 vector
  (sha1 35832f3b7bd6dbeb8d3b5c92f73b2f06759d2e7a)
  (range-size 1 17)
  (length-repr u8)
  4 u32)
```

The specification for a vector has a `length-repr` expression. This tells us
what type is used to encode the length of this vector. In this case, a `u8` is
used to encode the length. Decoding a single byte from our binary string yields
a value of `0x02`. Therefore, we know that our encoded vector contains two
elements. Since our element type is `u32`, we know to decode two `u32` values
from the binary string. This yields a vector of length 2 with the following value:

```
[ 0x000005F8, 0x000003AA ]
```

## Decoding a Union

The following is an encoded `union_unsigned`.

```
01 af 04
```

To interpret this, we can reference the `union_unsigned` definition in our
specification.

```
(type union_unsigned union
  (sha1 e59761d5c25294927e5026c278db565f7190b693)
  (range-size 2 9)
  (tag-repr u8)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
```

This tells us a few things: first, the encoded size will be between 2 and 9
bytes long. Our message is 3 bytes long. Next up, it tells us that the union's
tag will be represented as a `u8`. Finally, we see 4 fields are associated with
the union. Each field has an associated index. The value of the tag must match
one of these indices. The index that matches is the type that the union will
contain.

In our encoded message, we see that our first byte is `01`. We know that, since
we're decoding a union, this tag will match one of the field indices. As it
turns out, this index maps to the field `fu16`. This field is associated with
the `u16` type.

A `u16` is a primitive type. This means that it is a type that
contains an actual value. Furthermore, we know that that this type has
a fixed-size of 2. To decode a `u16` type, all we need to do is read
out 2 bytes from the binary stream

The next two bytes are `af 04`. We know that all Cauterize primitives are
expressed in little endian, so this yields the final hex value of 0x04AF, or
1199 in decimal.

At this point, there's nothing else to decode! We are out of bytes and the type
`union_unsigned` needs no more bytes to be complete.

Therefore, we can say that our `union_unsigned` value wraps a `u16` with the
value of 1199.

### Decoding a Combination

The following is an encoded `comb_unsigned`.

```
032cd506
```

To interpret this, we can reference the `comb_unsigned` type in our
specification.

```
(type comb_unsigned combination
  (sha1 d67b5d0a49e122140f418c12ad445ed013a52fc3)
  (range-size 1 16)
  (flags-repr u8)
  (fields
    (field fu8 u8 0)
    (field fu16 u16 1)
    (field fu32 u32 2)
    (field fu64 u64 3)))
```

Combinations use a set of flags to indicate which fields in the message are
encoded. The flags are always at the beginning of the message. We can look at
the `flags-repr` expression in the combination specification to determine how
wide the word used to represent the flags is. In the case of `comb_unsigned`,
the flags are represented as a `u8`.

We can see, based on the `flags-repr` expression in `comb_unsigned` that, the
flags in our current example are represented by the byte `0x03`--the first byte
of our message. Remember, if `flags-repr` was a differrent type, we'd use more
than one byte for our flags.

To start decoding fields in our combination, we start with the first field,
shift `1` to the left by the index of the field, and check whether or not the
bit is set in our flags. If the bit is set, we can then decode that type out of
the binary string. If the bit is not set, the field is skipped and we move on
to the next one.

We can see that our example has bits `(1 << 0)` (bit index 0) and `(1 << 1)`
(bit index 1) set. This means that our first two fields, `fu8` and `fu16` are
present in the binary string. The first field has type `u8` and the second
field has type `u16`. The field `fu8` deocdes as the value `0x2C` and the field
`fu16` decodes as the value `0x06d5` (remember, all builtin types are little
endian).

Our final type is, therefore, a `comb_unsigned` with the field `fu8` set to the
value `0x2C` and the field `fu16` set to the value `0x06d5`. The fields `fu32`
and `fu64` are not set in this encoded instance.

# Message Interface

TODO: Write about me.

# Answers to Obvious Questions

In this section, we'll try and justify a few of the obvious questions that come
up when reading this document. Cauterize has some odd restrictions, but they
are normally conscious decisions. If you have a question that you feel is
obvious and isn't listed here, feel free to open an issue with the question.

## Why isn't there a string type?

TODO: Answer this well. Strings are weird.

If your schema needs a string type, consider defining your own like this:

```
(schema string_example 1.0.0
  (vector utf8str8k u8 8192))
```

This is a vector of `u8` values. Its string encoding isn't checked, but it's
likely safe to assume that it should be valid UTF8 data based on the name.

## Why don't unions support multiple types per alternative?

In languages like Haskell, Rust, and OCaml, we're able to define union
types/sum types/algebraic alternative types that can contain multiple types per
constructor.

This behavior has not yet been supported in Cauterize because it adds
complexity to the C code that would need to be generated.

Take this hypothetical (but invalid) example:

```
(name "multi_data_union_example")
(version "1.0.0")

(type multi_type_field union
  (fields
   (field a u8 u16 u32)))
```

In Haskell, we might be able to expand this union expression into the following type:

```haskell
data MultiTypeField = A U8 U16 U32
```

In C, we'd need to do something like this:

```c
struct multi_type_field {
  enum multi_type_field_tag {
    multi_type_field_tag_a,
  } _tag;

  union {
    struct {
      u8 ix0;
      u16 ix1;
      u32 ix2;
    } a;
  };
};
```

There's no good way to express the names for different elements in the field.
We could come up with something, but it's not an obvious or clear path forward.
For this reason, we've chosen to omit multiple types per field in unions.

# TODO List

* Exhaustive checking for hash-collisions... just in case
* Hash algorithm selection
* Add a "ranged" type that's similar to a scalar but only accepts n..m values in a builtin
* Add ability to load schemas into other schemas
* Add small expression language to schemas for computing sizes or sharing numeric information
* Consider addition of generics to the schema
* Expand the things synonyms can refer to.

```
   )     _____             _            _
  /((   /  __ \           | |          (_)
 (_))\  | /  \/ __ _ _   _| |_ ___ _ __ _ _______
 _)((_) | |    / _` | | | | __/ _ \ '__| |_  / _ \
 \ ^ /  | \__/\ (_| | |_| | ||  __/ |  | |/ /  __/
  \_/    \____/\__,_|\__,_|\__\___|_|  |_/___\___|
```

# Cauterize

Cauterize is a data-description language that must always be able to target a
hard-real time embedded system without dynamic memory allocation.

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


### Primary Goals

From this goal, we can extract the following more specific goals:

* Must be achievable with static memory allocation - not all embedded systems
  support dynamic allocation.
* Must be achievable in bounded execution time - hard-real-time systems must
  know how long each operation can possibly take.
* Must have methods for detecting protocol drift early - embedded systems are
  often harder to update than desktop systems. They have longer deployment in
  more unusual conditions. Therefore, it is very important that the version of
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

### Secondary Goals

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

## Conventions

In this document the following terms have concrete meanings:

  * byte - an array of 8 bits of data

## Schema Language

The schema language uses parentheses to enclose each of its expressions. All
expressions have an assigned order for any and all arguments. Each expression
defines a new type with the exception of the top level expression. This top
level expression defines the outer details for the Cauterize schema.

```
(schema schema_name schema_version ...)
```

* `schema_name` - `[a-z]([a-z]|[0-9]|_)*`
* `schema_version` - `([a-z]|[0-9])([a-z]|[0-9]|_|.|-)*`

The reason the name and version patterns are restrictive is to ease the
burden on code generators. Some languages, such as Haskell and Ruby, have
specific rules about capitalization. Without the restrictive name pattern
Cauterize uses, code generators would have to do a lot more work to emit code
that is readable and matches the target language's normal conventions.

### Comments

Line comments are defined by using two `;` characters in a row. Here's an example.

```
(schema example 0.0.1
  ;; this is comment
  (some type definition))
```

### Built-In Types

There are several types that represent the foundation of the Cauterize types.
These are the fundamental types available for creating your own more complex
types. It is not possible to define new built-in types in a schema.

#### Unsigned Types

Unsigned values are encoded as little endian.

  * `u8` - 8 bits wide
  * `u16` - 16 bits wide
  * `u32` - 32 bits wide
  * `u64` - 64 bits wide

#### Signed Types

Signed values are encoded as [two's
complement](http://en.wikipedia.org/wiki/Two%27s_complement) [little
endian](http://en.wikipedia.org/wiki/Endianness) values.

  * `s8` - 8 bits wide
  * `s16` - 16 bits wide
  * `s32` - 32 bits wide
  * `s64` - 64 bits wide


#### Boolean Type

Booleans are encoded with a single byte. The only valid values are 0 and 1
where 0 represents `false` and 1 represents `true`.

  * `bool` - 8 bits wide

#### Floating Point Types

Floating point types are hard. Their definitions can be different (or missing
entirely!) across CPU architectures. Therefore, Cauterize only defines the
`f32`  and `f64` types. These are the [IEEE 754 single and double
precision floating point
values](http://en.wikipedia.org/wiki/IEEE_floating_point). The single precision
value uses 32 bits while the double-precision value uses 64 bits.

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
representation is identical to that of the built-in value they wrap.

```
(synonym [type name] [built-in type name])
```

The following example defines the type `age` that has the same representation
as a `u8`.

```
(schema example 1.0
  (synonym age u8))
```

#### Arrays

Arrays are fixed-length sequences of identially typed objects. These are to be
used when the sequence only makes sense with a fixed number of elements.

```
(array [type name] [element type] [array length)
```

Consider the following example that defines a type `mac` that encodes a Media
Access Control address (which is always 6 bytes long).

```
(schema example 1.0
  (array mac u8 6))
```

#### Vector

Vectors are variable-lengthed sequences of identically typed objects. These are
to be used when a sequence of elements has a maximum length, but may contain
fewer elements.

```
(vector [type name] [target type] [maximum array length])
```

The following example defines a generic byte buffer with a maximum length of
4096 bytes.

```
(schema example 1.0
  (vector byte_buffer_4k u8 4096))
```


#### Field Lists

Field lists cannot be defined on their own; they can only be used as the last
parameter to a `record`, `union`, or `combination` expression (which are
defined later in this document). Field lists are used to designate a set of
(name/type) pairs.

Field lists are defined like this:

```
(fields
  (field [field name] [optional type])
  (field ...))
```

A type is not required. The behavior of a type-less field is dependent on the
enclosing expression.

#### Records

Records are a collection of named fields where each field has a distinct type.

```
(record [type name] [field list])
```

An empty field in a record lacks any semantic meaning. It can neither be
encoded or represented by code generators.

This is an example of a record describing a person's age, height, and whether or
not they like cats:

```
(record person (fields (field age u8)
                       (field height u8))
                       (field likes_cats bool))
```

#### Union

Unions encode a set of possible values and some associated sub-value.  Like
records, their schema entries specify a list of fields, but, unlike records,
only one of those fields can represented in a union at any given time.

Unions in Cauterize are very similar to algebraic data types found in other
languages such as OCaml, Haskell, and Rust.

```
(union [type name] [field list])
```

An empty field in a union represents that a variant of the union is set. This
has meaning even if there is no associated data. A union where all fields lack
associated data behaves similarly to a C enumeration.

This example shows a `request` type for some key-value storage system. The
system stores `u64` values according to names that are up to 128 bytes long.

```
(schema example 1.0.0
  (vector key_name u8 128)
  (record key_pair (fields
                     (field name key_name)
                     (field value u64)))

  (union request (fields
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
(combination [type name] [field list])
```
As an example, consider the following description of a type capable of storing
changes in some observed values in a sensor rig:

An empty field in a Combination behaves like a boolean flag.

```
(combination sensed (fields
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

For a schema with a maximum length of 70,000: `(length-width 2)`.

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

The `flags-repo` expression represents the type used to encode a combination's
flags. It only occurs in `combination` specifications.

##### Example

```
(flags-repr u16)
```

#### `tag-repr` Expression

The `tag-repr` expression represents the type used to encode a union's type
tag. It only occurs in `union` specifications.

### `specification` Expression

All specification documents start with one top-level expression. It has the
following layout:

```
(specification [schema name] [schema version]
  (sha1 [a sha1 hash])
  (range-size [minimum encoded size] [maximum encoded size])
  (depth [maximum referential depth of the schema])
  (type-width [type-tag width hint])
  (length-width [length-tag width hint])
  [[type expression]])
```

### `builtin` Specification Expression

Built-In expressions are included for each built-in type directly or indirectly
used by the schema. If a built-in type is never used by the input schema, it is
not emitted in the specification.

All built-in expressions have the following layout:

```
(builtin [type name] (sha1 ...) (fixed-size ...))
```

### `synonym` Specification Expression

All synonym expressions have the following layout:

```
(synonym [type name] (sha1 ...) (fixed-size ...))
```

### `array` Specification Expression

All array expressions have the following layout:

```
(array [type name] (sha1 ...) (range-size ...)
  [array length] [element type])
```

### `vector` Specification Expression

All vectors have the following layout:

```
(vector [type name] (sha1 ...) (range-size ...)
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
(record [type name] (sha1 ...) (range-size ...)
  (fields ...))
```

### `union` Specification Expression

All unions have the following layout:

```
(record [type name] (sha1 ...) (range-size ...)
  (tag-repr [built-in type])
  (fields ...))
```

### `combination` Specification Expression

All combinations have the following layout:

```
(combination [type name] (sha1 ...) (range-size ...)
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
(specification binterp 0.0.0.0
  (sha1 ac9fda94cadb44d18af85d498f169609fd716efb)
  (range-size 1 17) (depth 2) (type-width 1) (length-width 1)
  (builtin u8
    (sha1 3c3c92ff20335765dbadd2930de367c0a8a9d9cb)
    (fixed-size 1))
  (builtin u64
    (sha1 ca58000caffa24364cf821488e348159a5d3ed11)
    (fixed-size 8))
  (builtin u32
    (sha1 13f56a24961b824565b27c3f7416dbd041ae6308)
    (fixed-size 4))
  (vector vec_u32
    (sha1 35832f3b7bd6dbeb8d3b5c92f73b2f06759d2e7a)
    (range-size 1 17)
    (length-repr u8)
    4 u32)
  (builtin u16
    (sha1 496042011a876c687fd713edb8388ab69e8b0bc6)
    (fixed-size 2))
  (union union_unsigned
    (sha1 e59761d5c25294927e5026c278db565f7190b693)
    (range-size 2 9)
    (tag-repr u8)
    (fields
      (field fu8 u8 0)
      (field fu16 u16 1)
      (field fu32 u32 2)
      (field fu64 u64 3)))
  (synonym syn_u32
    (sha1 f180b823f00f965e1f0f68ba5c82400f2d9dd32a)
    (fixed-size 4)
    u32)
  (record rec_unsigned
    (sha1 b58dd55deef9faf22ac07ced17cf6f87d1c95111)
    (range-size 15 15)
    (fields
      (field fu8 u8 0)
      (field fu16 u16 1)
      (field fu32 u32 2)
      (field fu64 u64 3)))
  (combination comb_unsigned
    (sha1 d67b5d0a49e122140f418c12ad445ed013a52fc3)
    (range-size 1 16)
    (flags-repr u8)
    (fields
      (field fu8 u8 0)
      (field fu16 u16 1)
      (field fu32 u32 2)
      (field fu64 u64 3)))
  (array arr_u32
    (sha1 965f3610970341adb1132d27a668a4c94e9e3d57)
    (range-size 16 16)
    4 u32))
```

Using this document and the knowledge of what type where trying to decode, we
can decode any encoded message for a particular specification.

It's important to remember that an encoded type, on its own, does not contain
enough information to identify it as that type. Two peers wanting to transcode
the same binary stream must agree on what type is being exchanged ahead of time
*or* have a method for identifying which type is being encoded on the wire.
Cauterize generators should normally emit the standard _message interface_
based off the `type-width` and `length-width` parameters in the specification.

In the following exercises, all encoded messages will be listed in hexadecimal.

## Decoding a BuiltIn

TODO: Write about me.

## Decoding an Array

TODO: Write about me.

## Decoding a Record

TODO: Write about me.

## Decoding a Vector

TODO: Write about me.

## Decoding a Union

The following is an encoded `union_unsigned`.

```
01 af 04
```

To interpret this, we can reference the `union_unsigned` definition in our
specification.

```
(union union_unsigned
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

Once again, we'll refer to our specification to determine how to decode a `u16`
type.

```
(builtin u16
  (sha1 496042011a876c687fd713edb8388ab69e8b0bc6)
  (fixed-size 2))
```

A `u16` is a builtin type. This means that it is a type that contains an actual
value. Furthermore, we can see that this type has a fixed-size of 2. To decode
a `u16` type, all we need to do is read out 2 bytes from the binary stream

The next two bytes are `af 04`. We know that all Cauterize primitives are
expressed in little endian, so this yields the final hex value of 0x04AF, or
1199 in decimal.

At this point, there's nothing else to decode! We are out of bytes and the type
`union_unsigned` needs no more bytes to be complete.

Therefore, we can say that our `union_unsigned` value wraps a `u16` with the
value of 1199.

### Decoding a Combination

TODO: Write about me

# Message Interface

TODO: Write about me.

# Answers to Obvious Questions

In this section, we'll try and justify a few of the obvious questions that come
up when reading this document. Cauterize has some odd restrictions, but they
are conscious decisions. If you have a question

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
(schema multi_data_union_example 1.0.0
  (union multi_type_field
    (fields
      (field a u8 u16 u32))))
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


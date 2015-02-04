# Cauterize

Cauterize is a data-description language that must always be able to target a
hard-real time embedded system without dynamic memory allocation.

## Introduction

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

Cauterize is _first_ intended to serve the constraints of hard-real-time
embedded systems programming, but is still suitable for a variety of other
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

### Primary Goals

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

### Secondary Goals

* Ease of implementation - code generators should be able to represent the
  specification in idioms common in the target language. In C, this is structs,
  enumerations, and unions. In Ruby, this would likely be classes. Furthermore,
  code generators should not have to generate any overly-complicated structures
  to conform to a specification. When a Cauterize feature is proposed, it must
  be implementable in simple terms in a variety of languages.
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

NOTE: The reason the name and version patterns are restrictive is to ease the
burden on code generators. Some languages, such as Haskell and Ruby, have
specific rules about capitalization. Without the restrictive name pattern
Cauterize uses, code generators would have to do a lot more work to emit code
that is readable and matches the target language's normal conventions.

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

Floating point types are hard. Their definitions can be different (or missing
entirely!) across CPU architectures. Therefor, Cauterize only defines the
`f32`  and `f64` types. These are the [IEEE 754 single and double
precision floating point
values](http://en.wikipedia.org/wiki/IEEE_floating_point). The single precision
value uses 32 bits while the double-precision value uses 64 bits.

#### Floating Point Types

Floating point types:

  * `f32` - 32 bits wide, IEEE754 Single Precision
  * `f64` - 64 bits wide, IEEE754 Double Precision

#### UTF Code Units

There are three UTF Code Unit built-in types defined. *These types do not
enforce their content to be valid UTF data. They merely act as a hint to
code-generators that the contents may be UTF data.* It is the responsibility of
the code generator to decide whether to validate any UTF data stored in
collections of code units.

  * `cu8` - 8 bit wide UTF code unit
  * `cu16` - 16 bit wide UTF code unit
  * `cu32` - 32 bit wide UTF code unit

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
  (vector key_name cu8 128)
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

## Specifications

TODO: Write about specifications.

# Answers to Obvious Questions

In this section, we'll try and justify a few of the obvious questions that come
up when reading this document. Cauterize has some odd restrictions, but they
are conscious decisions. If you have a question

## What's up with the weird UTF built-in types?

Good question: we could have the schema definition force code generators to
validate their UTF data. We could even have included a native string type in
the native schema types. We didn't do this precisely because of the load it
puts on code generation. It's unreasonable to expect that small embedded
systems should validate UTF data.

If your schema needs a string type, consider defining your own like this:

```
(schema string_example 1.0.0
  (vector utf8str8k cu8 8192))
```

This is a vector of `cu8` values. While this isn't forced to be UTF8 data, it
is likely a valid assumption that it should be. Code generators for different
targets are free to validate this data on their own OR have the programs that
use the generated code do the validation.

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


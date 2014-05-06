# Cauterize

## Schemas

### Types

#### BuiltIn

There are several types that represent the foundation of the Cauterize types.
These are the funadmental types available for creating your own more complex
types.

Integral types are assumed to be little endian on the wire.

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

#### FixedArrays

FixedArrays are types that allow the expression of a sequence of identical
types where the sequence only ever makes sense with a fixed number of members.
Consider a MAC address (always 6 bytes) or CAN-bus message payload ((almost)
always 8 bytes).

```scheme
(fixed [type name] [target type] [array length)
```

As an example, consider this definition of a FixedArray describing a MAC
address:

```scheme
(fixed macAddress u8 6)
```

#### BoundedArrays

BoundedArrays are types that allow the expression of a sequence of identical
types up to a maximum length. BoundedArrays are like FixedArrays in every way
except that their length must be checked in packing and unpacking.

```scheme
(bounded [type name] [target type] [maximum array length])
```

Consider this definition for a generic byte buffer with a maximum length of
4096 bytes.

```scheme
(bounded byteBuffer4k u8 4096)
```

#### Structures

Structures are like C structures in every way. They allow the description of a
type that has an ordered list of name-type pairs. These allow the expression of
more complex structures that are only meaningful with multiple values.

The description of a Structure follows the following pattern:

```scheme
(struct [type name] [list of fields])
```

where `[list of fields]` is a space-separated list of the following:

```scheme
(field [field name] [field type])
```

This allows us to define types like the following `user` type:

```scheme
(struct user (field name string64)
             (field yearOfBirth u16)
             (field monthOfBirth u8)
             (field dayOfBirth u8))
```

#### Enumerations

Enumerations are more akin to Enumerations one might find in Haskell or Rust.
That is, they are able to express a possible value in a type, but they are also
able to express values that can contain other values. Each instance of an
enumeration type can only ever contain one of its possible value variants at
any given time.

Enumerations, in C, are represented as tagged unions. That is, a struct that
associates a type tag together with a union type. The type tag needs to be
checked at runtime to determine which type variant is currently instantiated.

```scheme
(enum [type name] [variant list])
```

where `[variant list]` is a space-separated list of the following:

```scheme
(var [type name] [[optional contained type]])
```

Consider the following example for a 'request message' to some key-value
storage service:

```scheme
(enum request (var getKeyCount)
              (var checkKeyExists keyName)
              (var getKey keyName)
              (var eraseKey keyName)
              (var setKey keyValuePair))
```

Noe that the `getKeyCount` variant does not contain any assocaited data while
the `getKey` variant specifies a type that encodes the name of the key to get.

#### Sets

Sets are a closed collection of key-type pairs. The difference between Sets and
Structures is that the Set can, at any given time, only include *some* of its
possible members. The presense of encoded members is stored as a bitfield
encoded before any contained data.

```scheme
(set [type name] [list of fields])
```

where `[list of fields]` is a space-separated list of the following:

```scheme
(mem [field name] [field type])
```

As an example, consider the following description of a type capable of storing
changes in some observed values in a sensor rig:

```scheme
(set sensed (mem ambientTemp u16)
            (mem ambientLight u16)
            (mem airPressure u16)
            (mem positionX u32)
            (mem positionY u32)
            (mem positionZ u32))
```

If no sensor difference is detected in a specific sensor in a given time slice,
the value won't be included in the serialized value. This allows users to
encode values that only have deltas in a space-efficient way.

#### Padding

Padding types can be used to insert null bits into a payload. Padding types
must be 0 in the stream. Any other value will result in a pack/unpack error.

```scheme
(pad [type name] [padding width in bits])
```

Note that we represent padding with as bits. Currently, this must be
represented as multiples of 8 bits. Any value that is not evenly divisible by 8
is an error.

The following defines a type that can only be represented by 8 null bits.

```scheme
(pad p8 8)
```

#### Partial

Partials allow code generators to emit partial implementations of protocol
specifications. This can be useful if there are several small devices that are
to be part of a larger federated system. If there's a central node that
broadcasts messages to many nodes, but not all nodes are interested in all
messages, then Partial types allow the smaller nodes to only include packing
and unpacking code for the portions of the partial they use.

Partials use a hash of each variant's contained type as the type tag in the
serialization stream rather than an incrementing value as is used in an
enumeration. This makes the Parital types order-independent in their definition
and allows only part of the partial to be implemented by any given node. This
also includes the possibility for extension of the type in the future.

Partials have only two major semantic differences from enumerations: the
contained type on each variant is required rather than optional and the maximum
length of any contained type must be specified.

```scheme
(partial [type name] [maximum contained type length in bytes] [variant list]) 
```

Here's the same example demonstrated for Enumerations, but changed to meet the
requirements of a Partial.

```scheme
(partial request 1024 (var getKeyCount)
                      (var checkKeyExists keyName)
                      (var getKey keyName)
                      (var eraseKey keyName)
                      (var setKey keyValuePair))
```


## Specifications

```scheme
(name "cauterize")
(version "0.1.0")
(hash "22596363b3de40b06f981fb85d82312e8c0ed511")
```

# TODO List

* Exhaustive checking for hash-collisions.
* Hash algorithm selection.
* Prune unused builtins out of specifications.
* Note that specifications must list their dependencies in topographical order.

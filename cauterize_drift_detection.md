# Cauterize version drift detection

Cauterize creates a structural hash of all meaningful schema information. This
can be used to detect, at runtime, whether two communicating partners are
running the same version of the protocol or if they have drifted from
eachother. This approach does not rely on a developer remembering to update a
string version.

Below are two schemas that have identical lengths, names, and fields. The only
difference is the order in which the fields `x` and `y` occur.

```
(schema mismatch 1.0
  (record pair
    (fields
      (field x u32)
      (field y u32))))
```

```
(schema mismatch 1.0
  (record pair
    (fields
      (field y u32)
      (field x u32))))
```

This sort of field mis-match is hard to track down at run time because both
fields encode the same type of scalar information and have identical
representations when encoded.

Below, we see both of the specifications that result from running the Cauterize
specification compiler on our schemas.  Note that both the top level hash of
the schema and the hash of the `pair` type are different in each. Everything
else is the same.

```
(specification mismatch 1.0 (sha1 c2be7e466b7544161bb55c2a3ecafe0720f700fd) (range-size 4 8) (depth 2)
  (builtin u32 (sha1 13f56a24961b824565b27c3f7416dbd041ae6308) (fixed-size 4))
  (record pair (sha1 2bfa3f587c8eb270d574c012a379fa0463f41c2e)
    (range-size 8 8)
    (fields
      (field x u32 0)
      (field y u32 1))))
```

```
(specification mismatch 1.0 (sha1 9ca2659b023b6260fbf1a290eef4c6ce46ae0b48) (range-size 4 8) (depth 2)
  (builtin u32 (sha1 13f56a24961b824565b27c3f7416dbd041ae6308) (fixed-size 4))
  (record pair (sha1 d8ded099911f224a9f44917db71a6f365a3cc520)
    (range-size 8 8)
    (fields
      (field y u32 0)
      (field x u32 1))))
```

This top-level hash can be traded at connection time to establish protocol
compatability.

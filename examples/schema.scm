(schema a_name 0.0.1
        (scalar number64 s64)
        (scalar unsigned8 u8)
        (array somearray number64 64)
        (vector somevector number64 64)
        (struct astruct
                (fields
                  (field z somevector)
                  (field a s8)
                  (field d bstruct)))
        (struct bstruct
                (fields
                  (field a s8)
                  (field d cstruct)))
        (struct cstruct
                (fields
                  (field a s8)
                  (field b s8)))
        (enum anenum
              (fields
                (field a)
                (field b)
                (field c s8)
                (field d number64)))
        (set aset
                (fields
                  (field a number64)
                  (field b s8)
                  (field c anenum))))

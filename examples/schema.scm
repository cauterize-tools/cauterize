(schema "aName" "0.0.1"
        (scalar number s64) 
        (scalar unsigned u8)
        (const smallconst u8 34)
        (fixed somefixed number 64)
        (bounded somebounded number 64)
        (pad p8 8)
        (struct astruct
                (fields
                  (field z somebounded)
                  (field a s8)
                  (field padding p8)
                  (field b smallconst)
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
                (field d number)))
        (set aset
                (fields
                  (field a number)
                  (field b s8)
                  (field c anenum)))
        (partial req
                 (fields
                   (field a s32)
                   (field b s32))))

(schema "aName" "0.0.1"
        (scalar number s8) 
        (scalar unsigned u8)
        (const smallconst u8 34)
        (fixed somefixed number 64)
        (bounded somebounded number 64)
        (pad p8 8)
        (struct astruct
                (field a s8)
                (field padding p8)
                (field b smallconst)
                (field d bstruct))
        (struct bstruct
                (field a s8)
                (field d cstruct))
        (struct cstruct
                (field a s8)
                (field b s8))
        (enum anenum
              (field a)
              (field b)
              (field c s8)
              (field d number))
        (set aset
             (field a number)
             (field b s8)
             (field c anenum))
        (partial req (field a s32)
                     (field b s32)))

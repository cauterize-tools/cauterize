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
              (var a)
              (var b)
              (var c s8)
              (var d number))
        (set aset
             (mem a number)
             (mem b s8)
             (mem c anenum))
        (partial req 16 (var a s32)
                        (var b s64)))

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
                (field b smallconst))
        (enum anenum
              (var a)
              (var b)
              (var c s8)
              (var d number))
        (set aset
             (mem a number)
             (mem b s8)
             (mem c anenum)))

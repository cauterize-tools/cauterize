(schema "aName" "0.0.1"
        (scalar number s8) 
        (scalar unsigned u8)
        (const smallconst u8 34)
        (fixed somefixed number 64)
        (bounded somebounded number 64)
        (struct aname
                (field a s8)
                (field b smallconst))
        (enum aname
              (var a)
              (var b)
              (var c s8)
              (var d number)))

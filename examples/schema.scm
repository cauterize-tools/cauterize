(schema a_name 0.0.1
        (synonym number64 s64)
        (synonym unsigned8 u8)
        (array somearray number64 64)
        (vector somevector number64 64)
        (record arecord
                (fields
                  (field z somevector)
                  (field a s8)
                  (field d brecord)))
        (record brecord
                (fields
                  (field a s8)
                  (field d crecord)))
        (record crecord
                (fields
                  (field a s8)
                  (field b s8)))
        (union a_union
              (fields
                (field a)
                (field b)
                (field c s8)
                (field d number64)))
        (combination a_combination
                (fields
                  (field a number64)
                  (field b s8)
                  (field c a_union))))

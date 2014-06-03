(schema "bathroom monitor" "0.0.0"
        (enum status
              (fields
                (field open)
                (field closed)))
        (struct aoBathrooms
                (fields
                  (field upstairs status)
                  (field downstairs status))))

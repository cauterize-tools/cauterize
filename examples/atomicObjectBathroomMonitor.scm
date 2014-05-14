(schema "bathroom monitor" "0.0.0"
        (enum status
              (field open)
              (field closed))
        (struct aoBathrooms
                (field upstairs status)
                (field downstairs status)))

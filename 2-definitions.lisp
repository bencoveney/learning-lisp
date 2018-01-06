; Global parameter - usually use asterisks around identifier like *brian*
(defparameter message "Buh!")

(setf message "Bellow!")

; Global function
(defun badonde ()
    (print message))

(badonde)

; Local variables
(let
    (
        (a 2)
        (b 4))
    (print
        (+ a b))
)

; Local functions
(flet
    (
        (increment (n)
            (+ n 1))
        (double (n)
            (* n 2)))
    (print
        (increment (double 4)))
)

; Local functions that referenceabout each other (and themselves)
(labels
    (
        ; Determined by random dice roll
        (get-random-number () 4)
        ; Hides the original number
        (obfuscate (n)
            (+ n (get-random-number)))
    )
    (print (obfuscate 2))
)

; Tell clisp we are going to be printing weird lists
(setf *print-circle* t)

; Smoosh a list into the end of itself to create a list
(defparameter foo '(1 2 3))
(setf (cdddr foo) foo)

(print foo)

; Associative lists
(defparameter *drink-order* '(
    (bill . double-espresso)
    (lisa . small-drip-coffee)
    (john . medium-latte)))

; Find an association
(print (assoc 'lisa *drink-order*))

; Update an entry
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

; Find it again. Assoc assumes the first match is the correct one
; This makes all changes still available
(print (assoc 'lisa *drink-order*))

; Tree-like data
(defparameter *house*
    '(
        (walls
            (mortar
                (cement)
                (water)
                (sand))
            (bricks))
        (windows
            (glass)
            (frame)
            (curtains))
        (roof
            (shingles)
            (chimney))))

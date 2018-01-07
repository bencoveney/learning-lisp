; Association lists - map symbols to lists
; Use symbols rather than strings - easier to manipulate

; Places you can visit
(defparameter *nodes* '(
    (living-room (You are in the living-room. A wizard is snoring loudly on the couch.))
    (garden (You are in a beautiful garden. There is a well in front of you.))
    (attic (You are in the attic. There is a giant welding torch in the corner.))))

; Gets the description of a location from the associated list of places
(defun describe-location (location nodes)
    (cadr (assoc location nodes)))

; Connections between the places
(defparameter *edges* '(
    (living-room (garden west door) (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

; Quasiquoting - list is in "data" mode but can be flipped into "code" mode using the comma

; Gets the description of a connection
(defun describe-path (edge)
    `(there is a  ,(caddr edge) going ,(cadr edge) from here.))

; #' is shorthand for the function operator
; #'car => (function car)

; Describe multiple paths at once
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(print (describe-paths 'living-room *edges*))

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

(print (describe-location 'living-room *nodes*))

; Connections between the places
(defparameter *edges* '(
    (living-room (garden west door) (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

; Quasiquoting - list is in "data" mode but can be flipped into "code" mode using the comma

; Gets the description of a connection
(defun describe-path (edge)
    `(there is a  ,(caddr edge) going ,(cadr edge) from here.))

(print (describe-path (cadar *edges*)))

; #' is shorthand for the function operator
; #'car => (function car)

; Describe multiple paths at once
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(print (describe-paths 'living-room *edges*))

; Objects in the game
(defparameter *objects* '(whiskey bucket from chain))

; Locations of objects in the game
(defparameter *object-locations* '(
    (whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)))

; Finds the objects at the specified location
(defun objects-at (location objects object-locations)
    (labels
        (
            (at-location-predicate (object)
                (eq (cadr (assoc object object-locations)) location)))
        (remove-if-not #'at-location-predicate objects)))

(print (objects-at 'living-room *objects* *object-locations*))

; Describes the objects at a specified location
(defun describe-objects (location objects object-locations)
    (labels

        (
            (describe-object (object) `(you see a ,object on the floor.)))
        (apply #'append (mapcar #'describe-object (objects-at location objects object-locations)))))

(print (describe-objects 'living-room *objects* *object-locations*))

; Current location - global variable
(defparameter *location* 'living-room)

; Compiles all descriptions at the current location - accesses globals
(defun look ()
    (append
        (describe-location *location* *nodes*)
        (describe-paths *location* *edges*)
        (describe-objects *location* *objects* *object-locations*)))

(print (look))

; Walk in the specified direction
(defun walk (direction)
    (let
        (
            ; Find the next edge from the current location
            (next
                ; (find a b c d)
                ; Look in list b for an element that has a in the key of the cadr position
                ; e.g. look for the edge that has 'west as the 2nd symbol
                (find
                    direction
                    ; Edges for the current location
                    (cdr (assoc *location* *edges*))
                    :key
                    #'cadr)))
        (if next
            ; If we found an edge, walk along it
            (progn
                (setf *location* (car next))
                (look))
            '(You cannot go that way.))))

(print (walk 'west))

; Picks up the specified object at the current location
(defun pickup (object)
    (cond
        (
            ; Check the object is in the list of objects at the current location
            (member object (objects-at *location* *objects* *object-locations*))
            ; Add a link between the object and your inventory to the list of object locations
            ; The object locations will contain 2 entries for the object but assoc will give us the first
            (push (list object 'body) *object-locations*)
            `(You are now carrying the ,object))
        (
            T
            (You cannot get that.))))

(walk 'east)

(print (pickup 'whiskey))

; Shows the current inventory
(defun inventory ()
    (cons 'items- (objects-at 'body *objects* *object-locations*)))

(print (inventory))

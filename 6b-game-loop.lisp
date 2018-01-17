(defun basic-repl ()
    (loop (print (eval (read)))))

(defun game-repl ()
    (let
        (
            (command (game-read)))
        (unless
            (eq (car command) 'quit)
            (game-print (game-eval command))
            (game-repl))))

; Reads in the user's command
; Input in format: walk east
; Will be mapped to: (walk 'east)
(defun game-read ()
    (let
        (
            ; Wrap it in brackets so they can write it in plain text
            (command (read-from-string (concatenate 'string "(" (read-line) ")"))))
        (flet
            (
                ; Prepends a quote to the front of the argument
                ; 'foo is the same as (quote foo)
                (quote-it (argument)
                    (list 'quote argument)))
            ; Compile the result
            ; Leave the first argument as a command but map the rest to data mode
            (cons (car command) (mapcar #'quote-it (cdr command))))))

; List of allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory))

; Evaluates a command
(defun game-eval (sexp)
    (if
        ; Check the command is allowed
        (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(I do not know that command.)))

; Converts a list of characters to sentence case
(defun tweak-text (characters capitalise inside-quotes)
    (when characters
        (let
            (
                (item (car characters))
                (rest (cdr characters)))
            (cond
                ; Don't do any processing for spaces
                (
                    (eq item #\space)
                    (cons item (tweak-text rest capitalise inside-quotes)))
                ; If we read punctuation the next character should be capitalised
                (
                    (member item '(#\! #\? #\.))
                    (cons item (tweak-text rest t inside-quotes)))
                ; Some symbols will be quoted (to include a comma or other weird character)
                ; Toggle a flag to prevent subsequent iterations doing processing
                ; Don't include the quote in the output
                (
                    (eq item #\")
                    (tweak-text rest capitalise (not inside-quotes)))
                ; If we are inside some quotes then don't do any processing
                (
                    inside-quotes
                    (cons item (tweak-text rest nil inside-quotes)))
                ; Convert to uppercase
                (
                    (or capitalise inside-quotes)
                    (cons (char-upcase item) (tweak-text rest nil inside-quotes)))
                ; Convert to lowercase by default
                (
                    t
                    (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (output)
    ; Print it to the console on a new line
    (princ
        ; Convert the character list back to a string
        (coerce
            (tweak-text
                ; Convert the string into a list of characters
                (coerce
                    (string-trim
                        "() "
                        ; Convert the output to a string
                        (prin1-to-string output))
                    'list)
                t
                nil)
            'string))
    (fresh-line))

; Text game content - copy pasted -------------------------------------------------------------

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

; Gets the description of a connection
(defun describe-path (edge)
    `(there is a  ,(caddr edge) going ,(cadr edge) from here.))

; Describe multiple paths at once
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; Objects in the game
(defparameter *objects* '(whiskey bucket chain frog))

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

; Describes the objects at a specified location
(defun describe-objects (location objects object-locations)
    (labels
        (
            (describe-object (object) `(you see a ,object on the floor.)))
        (apply #'append (mapcar #'describe-object (objects-at location objects object-locations)))))

; Current location - global variable
(defparameter *location* 'living-room)

; Compiles all descriptions at the current location - accesses globals
(defun look ()
    (append
        (describe-location *location* *nodes*)
        (describe-paths *location* *edges*)
        (describe-objects *location* *objects* *object-locations*)))

; Walk in the specified direction
(defun walk (direction)
    (let
        (
            (next
                (find
                    direction
                    (cdr (assoc *location* *edges*))
                    :key
                    #'cadr)))
        (if next
            (progn
                (setf *location* (car next))
                (look))
            '(You cannot go that way.))))

; Picks up the specified object at the current location
(defun pickup (object)
    (cond
        (
            (member object (objects-at *location* *objects* *object-locations*))
            (push (list object 'body) *object-locations*)
            `(You are now carrying the ,object))
        (
            T
            '(You cannot get that.))))

; Shows the current inventory
(defun inventory ()
    (cons 'items- (objects-at 'body *objects* *object-locations*)))

; -------------------------------------------------------

; Run the repl
(game-repl)

; Wizard text game content - copy pasted -------------------------------------------------------------

; Places you can visit
(defparameter *wizard-nodes* '(
    (living-room (You are in the living-room. A wizard is snoring loudly on the couch.))
    (garden (You are in a beautiful garden. There is a well in front of you.))
    (attic (You are in the attic. There is a giant welding torch in the corner.))))

; Connections between the places
(defparameter *wizard-edges* '(
    (living-room (garden west door) (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

; --------------------------------------------------------------------------------------

; Convert an expression to a valid DOT identifier
(defun dot-name (expression)
    ; Replace any characters that match the test
    ; Can be used on lists as well
    (substitute-if
        #\_
        ; Complement will invert to identify any characters that are NOT alphanumeric
        (complement #'alphanumericp)
        (prin1-to-string expression)))

(print (dot-name 'living-room))

(defparameter *max-label-length* 30)

; Convert to a valid DOT label
(defun dot-label (expression)
    (if
        expression
        (let
            (
                ; Convert the expression to a string
                ; Specify that we will be passing the "pretty" paramter, and turn it off
                ; We don't want new line characters or tabs etc
                (as-string (write-to-string expression :pretty nil)))
            (if
                ; If the string is too long
                (> (length as-string) *max-label-length*)
                ; Chop it down and append an ellipsis
                (concatenate 'string (subseq as-string 0 (- *max-label-length* 3)) "...")
                ; Use it as is
                as-string))
        ""))

(print (dot-label '(ben is cool)))

; Encodes nodes as DOT names and labels
(defun nodes->dot (nodes)
    (mapc
        (lambda (node)
            (fresh-line)
            (princ (dot-name (car node)))
            (princ "[label=\"")
            (princ (dot-label node))
            (princ "\"];"))
        nodes))

(nodes->dot *wizard-nodes*)

; Encodes edges as DOT edges
(defun edges->dot (edges)
    (mapc
        (lambda (node)
            (mapc
                (lambda (edge)
                    (fresh-line)
                    (princ (dot-name (car node)))
                    (princ "->")
                    (princ (dot-name (car edge)))
                    (princ "[label=\"")
                    (princ (dot-label (cdr edge)))
                    (princ "\"];"))
                (cdr node)))
        edges))

(edges->dot *wizard-edges*)

; Creates the complete DOT document
(defun graph->dot (nodes edges)
    (princ "digraph{")
    (nodes->dot nodes)
    (edges->dot edges)
    (princ "}"))

(graph->dot *wizard-nodes* *wizard-edges*)

; Convert the DOT file to PNG
; Thunk (or suspension) is a nullary function - has no arguments and is used to defer execution
(defun dot->png (file-name thunk)
    ; Create a stream variable that will write to a file, then write to the stream
    (with-open-file
        (
            ; The alias that can be used to refer to the stream
            ; Standard output is a special one that will be the default target of print output
            ; Within this function context we can overwrite *standard-output* to be our own stream.
            *standard-output*
            ; Where to write to.
            file-name
            ; Keyword parameter - :name :value
            ; Colon prefix is a symbol that can't be reassigned (keyword symbol)
            ; We are writing to a file
            :direction :output
            ; Overwrite an existing file
            :if-exists :supersede)
        (funcall thunk))
    (ext:shell
        (concatenate 'string "dot -Tpng -O " file-name)))

; Create the full graph and create a png
(defun graph->png (file-name nodes edges)
    (dot->png
        file-name
        (lambda () (graph->dot nodes edges))))

(graph->png "graphs\\wizard.dot" *wizard-nodes* *wizard-edges*)

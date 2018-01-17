; Printing lines
(print
    (progn
        (print "this")
        (print "is")
        (print "a")
        (print "test")))

; Printing to one line
(print
    (progn
        (prin1 "this")
        (prin1 "is")
        (prin1 "a")
        (prin1 "test")))

; Read a name and greet the user
(defun say-hello ()
    (print "Type your name (in quotes):")
    (let
        (
            ; Read a value and assign it to the name variable
            (name (read)))
        (print "Nice to meet you, ")
        (print name)))

(say-hello)

; Printing other values. | can create special symbols
(print 5)
(print 5.5)
(print "five")
(print 'five)
(print '|CaseSensitiveSymbol|)
(print '|A Valid Lisp Symbol?!?!|)

; Read will ready anything (literals, code) so we can add numbers by omitting quotes
(print "write a number")
(print (+ 5 (read)))

; princ will text without quotes like you'd expect. #\ creates prints characters
(princ 5)
(princ 5.5)
(princ "five")
(princ 'five)
(princ '#\a)
(princ '#\space)
(princ '#\tab)
(princ '#\newline)

; read-line will treat input as a string without quotes

; Read a name and greet the user
(defun better-hello ()
    (princ "Who are you?: ")
    (let
        (
            ; Read a value and assign it to the name variable
            (name (read-line)))
        (princ "Nice to meet you, ")
        (princ name)))

(better-hello)

; eval will run data as code
(defparameter *foo* '(+ 1 2))
(print (eval *foo*))

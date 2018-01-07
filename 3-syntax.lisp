; Symbols

; Case insensitive words
(print (eq 'buh 'BUH))

; Looks like T is just a symbol?
(print (eq T 'T))

; Numbers

; Integers
(print (+ 1 2)) ; 3

; Rational
(print (/ 4 6)) ; 2/3

; Floats
(print (+ 1.0 2)) ; 3.0
(print (/ 4.0 6)) ; 0.6666667

; Strings

; Princ
; Also returns the value
; Doesn't make a new line
(princ "Brian Badonde")
(princ "He said \"Buh\"")

; Code VS Data

; A list - Brackets with 0-n items of content:
(print '(Chicken Cow Sheep))

; Can create them using list function
(print (list 'Chicken 'Cow 'Sheep))

; Can create them using list function
(print '(Chicken (Brown-Cow Dairy-Cow) Sheep))

; A form - A list with a command at the beginning e.g. expt
(print (expt 2 3)) ; 8

; Single quote makes it just data - "quoting"
; First will be treated as identifier
(print '(expt 2 3)) ; (EXPT 2 3)

; Cons cells

; Point to two things

; Lists are made from nil terminated singly linked cons cell lists - Usually point to
; a: data
; b: another cell or nil

; Create one cons cell
(print (cons 'Chicken 'Egg)) ; (CHICKEN . EGG) - dot indicates just a cell, not a list

; Cons sell with nil is just a one-element list
(print (cons 'Chicken nil)) ; (CHICKEN)
(print (cons 'Chicken ())) ; (CHICKEN)

; Prepend to list
(print (cons 'Chicken '(Cow Sheep)))

; Verbose list building
(print (cons 'Chicken (cons 'Cow (cons 'Sheep ()))))

; CAR and CDR
; Contents of address register
; Contents of decrement register

; CAR gets first item - not as a list
(print (car '(Chicken Cow Sheep))) ; CHICKEN

; CDR gets rest
(print (cdr '(Chicken Cow Sheep))) ; (COW SHEEP)

; Can be combined up to 4 levels deep e.g. CADR = get rest, then get first of that
(print (cadr '(Chicken Cow Sheep))) ; COW

; Can access nested items e.g. get the rest, then get the first of that (un-nest), then get the first of that
(print (caadr '(Chicken (Brown-Cow Dairy-Cow) Sheep))) ; BROWN-COW

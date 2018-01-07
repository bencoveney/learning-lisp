; Falsy

; Empty list is the only false value.
(print
    (if '()
        'i-am-true
        'i-am-false)) ; <--

(print
    (if '(1)
        'i-am-true ; <--
        'i-am-false))

; Useful when "eating" lists e.g. calculating length
(print
    (labels
        (
            (get-length (list)
                (if list
                    (1+ (get-length (cdr list)))
                    0))
        )
        (get-length '(list with four symbols)))) ; 4

; Some other expressions are considered the same as an empty list (falsy)
(print (eq () '()))
(print (eq () nil))
(print (eq () 'nil))

; A quirk is that () is a "form" (not quoted) but doesn't have an initial command

; Conditionals

; If
(print (if (= (+ 1 2) 3) 'yes 'no)) ; yes
(print (if () 'empty 'filled)) ; empty
(print (if (oddp 3) 'odd 'even)) ; odd

; If is a special form - won't evaluate all parameters
(print (if (oddp 3) 'odd (/ 1 0))) ; odd

; progn - do multiple things inside a branch. Last thing is returned.
(print
    (if
        (oddp 3)
        (progn (print "almost done") 'odd)
        (progn (print "getting there") 'even)))

; when and unless - like a one-branched if with an implicit progn
(print
    (when
        (oddp 3)
        (print "almost done") ; Will print
        'odd)) ; returned

; nil returned
(print
    (unless
        (oddp 3)
        (print "almost done")
        'odd))

; cond - does it all
(print
    (cond
        (
            (eq 2 3)
            (print "No they are not")
            'not-returned)
        (
            (eq 3 3)
            (print "Yes they are")
            'returned)
        (
            T
            (print "Did we get this far?")
            'also-not-returned)))

; case statement
(print
    (case 'Brian
        (
            'Ed
            (print "I am not Brian")
            'not-returned)
        (
            'Brian
            (print "Buh!")
            'returned)
        (otherwise
            (print "Nothing was a match")
            'also-not-returned)))

; Boolean logic
(print (and (oddp 7) (oddp 9))) ; T
(print (and (oddp 7) (oddp 8))) ; nil

(print (or (oddp 7) (oddp 10))) ; T
(print (or (oddp 8) (oddp 10))) ; nil

(print T) ; T
(print (not T)) ; nil

; Shortcut evaluation
(print (and (oddp 8) (print "Not evaluated"))) ; nil

; Some functions will return extra data - result is still truthy
(print (member 1 '(3 4 1 5))) ; (1 5) - Returns cons cell containing the item
; If it returned the found item - what if we searched for nil in a list? result would be falsy.

; Can be useful for dual-role functions e.g. find-if can be used to find a value AND check if present
(print
    (if
        (find-if #'oddp '(3 4 1 5))
        'has-odd
        'no-odd))
; This one will break if looking for nil though

; Equality

; Use eq for symbols, equal for everything else
(print (eq 'yes 'YES)) ; Reference?
(print (equal "yes" "yes")) ; Value? Are they isomorphic

; Comparing lists
(print (eq '(Hello World) '(Hello World))) ; nil - not the same list
(print (equal '(Hello World) '(Hello World))) ; T - Lists have same content

; eql will match symbols AND values
(print (eql 'yes 'YES)) ; T
(print (eql "yes" "yes")) ; T

; equalp will perform conversions
(print (eql 1 1.0)) ; T
(print (eql "yes" "YES")) ; T

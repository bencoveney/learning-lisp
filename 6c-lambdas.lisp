; Classic way of defining a function
(defun half (n) (/ n 2))

; Defining an anonymous function
(lambda (n) (/ n 2))

; Using a lambda
(print (mapcar (lambda (n) (/ n 2)) '(2 4 6)))
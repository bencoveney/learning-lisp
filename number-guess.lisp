; Needs to be copied into the repl to run.
; Pick a number between 1 and 100 (inclusive)
; Use (smaller) and (bigger) if the guess was wrong or right.
; Use (start-over) when your number has been guessed.

(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
	(ash (+ *small* *big*) -1))

(defun smaller ()
	(setf *big* (1- (guess-my-number)))
	(guess-my-number))

(defun bigger ()
	(setf *small* (1+ (guess-my-number)))
	(guess-my-number))

(defun start-over ()
	(defparameter *small* 0)
	(defparameter *big* 100)
	(guess-my-number))

(guess-my-number)

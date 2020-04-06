(print 'foo)
(print "foo")
(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test")
       )

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "when I add five I get")
    (print (+ num 5))))

(print '#\a)
(princ '#\a)

(progn (print "This sentence is interrupted")
       (print #\newline)
       (print "by an annoying character."))

(progn (princ "This sentence is interrupted")
       (princ #\newline)
       (princ "by an annoying character."))

(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "nice to meet you, ")
    (princ name)))

(defparameter *foo* '(+ 1 2))

(eval *foo*)

(defun game-repl ()
  (loop (print (eval (read)))))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (princ cmd)
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil))))))) 

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "()" (prin1-to-string lst))
				     'list)
			     t nil)
		 'string))
  (fresh-line))


(game-print '(THAT IS A SENTENCE. WHAT ABOUT THIS? PROBABLY.))

(game-print '(THIS IS a test. there is "comma," but there is NOT double quotation.))


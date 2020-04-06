(defparameter *small* 1)
(defparameter *big* 100)
(defun guess_my_number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess_my_number)))
  (guess_my_number)
  )

(defun bigger ()
  (setf *small* (1+ (guess_my_number)))
  (guess_my_number)
  )

(defun start_over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess_my_number))

(let ((a 5)
      (b -3))
  (+ a b))


(flet ((f (n)
	  (+ n 10)))
  (f 5))

(flet (
       (f (n)
	  (+ n 10))
       (g (n)
	  (- n 3))
       )
  (g (f 5)))

(labels ((a (n)
	    (+ n 5))
	 (b (n)
	    (+ (a n) 6)))
  (b 10))


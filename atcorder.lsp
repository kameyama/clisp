;;problem A
(let ((a (read))
      (b (read)))
     (if (oddp (- a b))
         (princ "impossible")
         (princ (/ (+ a b) 2))
         ))


;;problem B
(defparameter *k* 1)
(defparameter *l* 0)

(defun cnt (lst)
  (if lst
      (progn (unless (equal (car lst) *k*)
	       (setf *l* (+ *l* 1)))
	     (setf *k* (+ *k* 1))
	     (cnt (cdr lst))
	     )
      *l*
      )
  )
 
      
	  
(let* ((n (read))
       (lst (loop :repeat n :collect (read)))
       )
(if (member (cnt lst) '(0 2))
    (princ "YES")
    (princ "NO")
    )
)

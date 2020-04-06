(if '()
    'i-am-true
  'i-am-false)

(if '(2)
    'i-am-true
  'i-am-false)

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
    0))

(my-length '(list with four symbols))

(1+ (my-length (cdr '(a) )))

(1+ (my-length '(a)))

(if '(a)
    (1+ 0)
  0)

(eq '() nil)

(eq '() ())

(eq 'nil nil)

(if (= (+ 1 2) 3)
    'yup
  'nope)

(if (= (+ 1 2) 4)
    'yup
  'nope)

(if (oddp 5)
    'odd-number
  'even-number)

(if (oddp 5)
    'odd-number
    (/ 1 0))

(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
	   'odd-number)
  'even-number)

(defvar *number-is-odd* nil)

(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)


(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)

(defvar *arc-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arc-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((eq person 'johnny) (setf *arc-enemy* 'unless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(t '(why you ate my pudding stranger ?)))
  )

(defun pudding-eater (person)
  (case person
    ((henry) (setf *arc-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((johnny) (setf *arc-enemy* 'unless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(otherwise '(why you ate my pudding stranger ?)))
  )

(and (oddp 5) (oddp 7) (oddp 9))

(or (oddp 5) (oddp 7) (oddp 9))

(defparameter *is-it-even* nil)

(or (oddp 4) (setf *is-it-even* t))

(or (oddp 5) (setf *is-it-even* t))

(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list
    )

(member 1 '(3 4 1 5))

(if (member nil '(3 4 nil 5))
    'one-is-in-the-list
    'one-is-not-in-the-list
    )

(member nil '(3 4 1 nil))

(if (member nil '(3 4 1 nil))
    'true
    'false)

(if '(nil)
    'true
    'false)

(if '(a)
    'true
    'false)

(if '()
    'true
    'false)

(find-if #'oddp '(2 4 5 6))(find-if #'oddp '(2 4 5 6))

(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number
    )

(find-if #'null '(2 4 nil 6))

(null nil)

(defparameter *fruit*'apple)

(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))

(equal 'apple 'apple)

(equal (list 1 2 3) (list 1 2 3))

(equal (list 1 2 3) (cons 1 (cons 2 (cons 3 ()))))

(eql 'foo 'foo)

(equalp "Bob Smith" "bob smith")

(equal "Bob Smith" "bob smith")

(equal 0 0.0)

(equalp 0 0.0)

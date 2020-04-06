(defparameter *nodes* '((living-room (you are in the living room.
				      a wizard is snoring loudly on the coach.))
                        (garden (you are in a beautiful garden.
				 there is a well in front of you.))
                        (attic (you are in the attic.
				there is a giant welding torch in the corner.))))

(caddr *nodes*)

(assoc 'garden *nodes*)

(defun describe-location (location nodes)
    (cadr (assoc location nodes))
    )

(describe-location 'living-room *nodes*)

(defparameter *edges* '((living-room (garden west room)
			(attic upstairs ladder))
		       (garden (living-room east door))
		       (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(cdr (assoc 'living-room *edges*))

(describe-paths 'living-room *edges*)


(mapcar #'describe-path '((GARDEN WEST ROOM) (ATTIC UPSTAIRS LADDER)))

(mapcar #'sqrt '(1 2 3 4 5 9))

(mapcar #'car '((foo bar) (baz qux)))

(mapcar (function car) '((foo bar) (baz qux)))

(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux)))
  )

(append '(mary had) '(a) '(little lamb))

(apply #'append '((mary had) (a) (little lamb)))


(apply #'append '((There is a door going west from here.)
		  (There is a ladder going upstairs from here.)))

(defparameter *objects* '(whisky bucket frog chain))

(defparameter *object-locations* '((whisky living-room) (bucket living-room) (frog garden) (chain gardem)))

(defun objects-at (loc objs obj-locs)
  (labels ( (at-loc-p (obj)
	      (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(objects-at 'living-room *objects* *object-locations*)

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(describe-objects 'living-room *objects* *object-locations*)

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(look)

(defun walk (direction)
  (let((next (find direction (cdr (assoc *location* *edges*))
		   :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)

(walk 'west)

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defparameter *foo* '(1 2 3))

(push 7 *foo*)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(inventory)

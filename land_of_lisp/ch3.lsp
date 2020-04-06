(eq 'fooo 'FoOo)

(+ 1.0 1)

(expt 53 53)

(/ 53 53)

(/ 2 1024)

(/ 4.0 6)

(princ "Hello World!")

(expt 2 3)

(expt 2 (+ 3 4))

'(expt 2 (+ 3 4))

(cons 'chicken 'cat)

(cons 'chicken 'nil)

(cons 'chicken ())

(eq (cons 'chicken 'nil) (cons 'chicken ()))

(cons 'pork '(beef chicken))

(cons 'pork (cons 'beef ()))

(car '(pork beef chicken))

(cdr '(pork beef chicken))

(car '(beef chicken))

(car (cdr '(pork beef chicken)))

(cadr '(pork beef chicken))

(list 'pork 'beef 'chicken)

(car '((pass carrots tomatoes) (pork beef chicken)))

(cdr '(pass carrots tomatoes))

(cdr (car '((pass carrots tomatoes) (pork beef chicken))))

(cadr '((pass carrots tomatoes) (pork beef chicken)))

(cdar '((pass carrots tomatoes) (pork beef chicken)))

(cddr '((peas carrot tomatoes) (pork beef chicken) duck))


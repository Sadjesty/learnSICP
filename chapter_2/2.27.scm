(define (deep-reverse a)
  (if (not (pair? a))
    a
    (reverse (map deep-reverse a))))
(define x (list 2 (list 1 (list 3 4 5) 2) 4 (list 3 4) (list 1 9 2 28)))
(deep-reverse x)
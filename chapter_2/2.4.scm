(define (my-cons x y)
    (* (fast-expt 2 x)
      (fast-expt 3 y)))
(define (my-car p)
    (if (not (= (remainder p 2) 0))
        0
          (+ (my-car (/ p 2)) 1)))
(define (my-cdr p)
    (if (not (= (remainder p 3) 0))
        0
          (+ (my-cdr (/ p 3)) 1)))
(my-car (my-cons 4 2))
(my-cdr (my-cons 3 6))
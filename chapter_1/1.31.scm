(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial-new n)
  (product identity 1 inc n))
;(factorial-new 5)
(define (pi-divided-by-four n)
  (define (term a)
    (/ (* (- a 1.0) (+ a 1.0))
       (square a)))
  (define (next a)
    (+ a 2))
  (product term 3.0 next n))
(* (pi-divided-by-four 1000000) 4)



(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
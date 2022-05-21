(define (improve-cube guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))
(define (cube-iter guess x)
  (define new-guess (improve-cube guess x))
  (if (alt-good-enough? guess new-guess)
      guess
      (cube-iter new-guess x)))
(define (cube-root x)
  (cube-iter 1.0 x))
(cube-root 27)

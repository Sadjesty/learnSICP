(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
(newtons-method (cubic 0 0 -8) 1)
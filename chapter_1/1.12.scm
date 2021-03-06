(define (pascal-triangle x y)
  (cond ((= y 0) 1)
        ((= y 1) 1)
        ((or (= x 0) (= x y)) 1)
        (else (+ (pascal-triangle (- x 1) (- y 1))
                 (pascal-triangle x (- y 1))))))
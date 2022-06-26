
(define (div-interval-v2 x y)
    (let (divisor (make-interval (/ 1.0 (upper-bound y))))
        (if (cross-zero? divisor)
            (error "Dividing by zero")
              (mul-interval x divisor))))


(define (make-point x y)
    (cons x y))
(define (x-point a)
    (car a))
(define (y-point a)
    (cdr a))
(define (make-segment a b)
    (cons a b))
(define (start-segment s)
    (car s))
(define (end-segment s)
    (cdr s))
(define (print-point p)
    (newline)
      (display "(")
      (display (x-point p))
      (display ",")
      (display (y-point p))
      (display ")"))
(define (mid-point s)
    (let ((a (start-segment s))
           (b (end-segment s)))
        (make-point (/ (- (x-point b) (x-point a)) 2)
          (/ (- (y-point b) (y-point a)) 2))))
(define A (make-point 1 2))
(define B (make-point 5 6))
(define AB (make-segment A B))

(define mp (mid-point AB))
(print-point mp)
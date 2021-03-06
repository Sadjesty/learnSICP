(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
    (/ (- (upper-bound i) (lower-bound)) 2))

(define (make-center-percent c p)
    (let ((width (/ (* c p) 100)))
        (make-interval (- c width) (+ c width))))
(define (percent i)
    (* (/ (width i)
         (center i))
      100))
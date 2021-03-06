(define (print-rat x)
      (newline)
      (display (numer x))
      (display "/")
      (display (denom x)))

(define (make-rat n d)
    (let ((g (abs (gcd n d))))
        (cond ((and (> n 0) (< d 0)) (cons (- (/ n g)) (abs (/ d g))))
          ((and (< n 0) (< d 0)) (cons (abs (/ n g)) (abs (/ d g))))
          (else (cons (/ n g) (/ d g))))))

(print-rat (make-rat 1 3))
(print-rat (make-rat -1 3))
(print-rat (make-rat 1 -3))
(print-rat (make-rat -1 -3))
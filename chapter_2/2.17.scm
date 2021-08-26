(define squares (list 1 4 9 16 25))

(define (last-pair list)
  (if (null? (cdr list))
    (car list)
    (last-pair (cdr list))))
    
(last-pair squares)
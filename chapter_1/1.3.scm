(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))
(define (sum-of-two-bigger-numbers a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c)
          (sum-of-squares a b)
          (sum-of-squares b c))))
(sum-of-two-bigger-numbers  5 6 3)
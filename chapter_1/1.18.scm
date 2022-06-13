(define (double a)
  (* a 2))
(define (halve a)
  (/ a 2))
(define (fast-mult a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))


(define (fast-mult-it a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))
  (iter a b 0))
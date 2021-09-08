(define odds (list 1 3 5 7))

(define (reverse a)
  (if (null? a)
    a
    (append (reverse (cdr a))
      (list (car a)))))

(reverse odds)
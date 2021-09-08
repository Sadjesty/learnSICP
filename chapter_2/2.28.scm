(define (fringe a)
  (define (fringe-builder given result)
    (cond ((null? given) result)
          ((not (pair? given)) (cons given result))
          (else (append (fringe-builder (car given) result) (fringe-builder (cdr given) result)))))
  (fringe-builder a nil))
      
(define x (list (list 1 2 (list 1 23 4)) (list (list (list 1))) (list 3 4)))

(fringe x)
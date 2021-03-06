(define square (lambda (x) (* x x)))
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (square (car items))
;            (square-list (cdr items)))))
;(define (square-list items)
;  (map square items))

;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square (car things))))))
;  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items nil))
(square-list (list 1 2 3 4))
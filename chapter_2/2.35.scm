(define (count-leaves1 tree)
  (accumulate +
              0
             (map (lambda (x) 
                    (if (not (pair? x))
                        1
                        (count-leaves1 x)))
                  tree)))
(define x (cons (list 1 2) (list 3 4)))
(define tree (list 1 2 (list 3 4) (list 5 (list 6 7)))) 
(count-leaves1 x)
(count-leaves1 tree)

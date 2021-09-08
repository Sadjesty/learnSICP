
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (structure? structure)
  (pair? structure))

(define (total-weight mobile)
  (if (not (structure? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (define (torque branch)
    (* (total-weight (branch-structure branch))
       (branch-length branch)))
  (define (torque-balance? mobile)
    (if (not (structure? mobile))
        true
        (= (torque (left-branch mobile))
           (torque (right-branch mobile)))))
  (cond ((or (null? mobile) (not (structure? mobile))) true)
        ((torque-balance? mobile) true)
        (else false)))

 (define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8))))) 
  
 ;;          4  |  5 
 ;;        +----+-----+ 
 ;;        6        3 |     9 
 ;;               +---+---------+ 
 ;;               7             8 
  
 ;  (total-weight m1) 
 ;  Value: 21 
 
 (define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4))))) 
  
 ;;          4  | 2 
 ;;        +----+--+ 
 ;;        6    5  |    10 
 ;;          +-----+----------+ 
 ;;          8                4 
  
 (balanced? m2) 
 ;Value: #t 
 (balanced? m1) 
 ;Value: #f 
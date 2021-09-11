(define (list-ref list n)
  (if (= n 0)
    (car list)
    (list-ref (cdr list) (- n 1))))

(define one-through-four (list 1 2 3 4))

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(define (length list)
  (if (null? list)
    0
    (+ 1 (length (cdr list)))))

(define(length-it list)
  (define (iter list result)
    (if (null? list)
      result
      (iter (cdr list) (+ 1 result))))
  (iter list 0))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) 
          (append (cdr list1) list2))))

(define (last-pair list)
  (if (null? (cdr list))
    (car list)
    (last-pair (cdr list))))


(define (reverse a)
  (if (null? a)
    a
    (append (reverse (cdr a))
      (list (car a)))))

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))

(define (append-element elements elem)
  (append elements (list elem))) 
(define (same-parity a . m)
  (define (parity? a b)
    (let ((a-par (remainder a 2))
          (b-par (remainder b 2)))
      (= a-par b-par)))
  (define (append-parity result-list arguments)
    (if (null? arguments)
        result-list
        (if (parity? (car result-list) (car arguments))
            (append-parity (append-element result-list (car arguments)) (cdr arguments))
            (append-parity result-list (cdr arguments)))))
  (append-parity (list a) m))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

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

(define (for-each proc items)
  (if (null? items)
      true
      (and (proc (car items))
           (for-each proc (cdr items)))))


(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;(define test (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr test)))))
;(define test (list (list 7)))
;(car (car test))
;(define test (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr test))))))))))))

;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(append x y) ;---> (1 2 3 4 5 6)
;(cons x y) ;---> ((1 2 3) 4 5 6)
;(list x y) ;---> ((1 2 3) (4 5 6))

(define (deep-reverse a)
  (if (not (pair? a))
    a
    (reverse (map deep-reverse a))))
(define (fringe a)
  (define (fringe-builder given result)
    (cond ((null? given) result)
          ((not (pair? given)) (cons given result))
          (else (append (fringe-builder (car given) result) (fringe-builder (cdr given) result)))))
  (fringe-builder a nil))



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
  
 ;balanced? m2) 
 ;Value: #t 
 ;balanced? m1) 
 ;Value: #f 
 
 
 
;(define (scale-tree tree factor)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;             (scale-tree sub-tree factor)
;             (* sub-tree factor)))
;       tree))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
;(define (square-tree-map tree)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;             (square-tree sub-tree)
;             (square sub-tree)))
;       tree))


(define (tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree proc)
             (proc sub-tree)))
  tree))

(define (scale-tree tree factor)
  (tree-map tree (lambda (x) (* factor x))))
(define (square-tree-map tree)
  (tree-map tree square))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (fib k)
  (if (< k 2)
      k
      (+ (fib (- k 1))
         (fib (- k 2)))))




(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))


(define (map1 p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length1 sequence)
  (accumulate (lambda (x y)
                (+ y 1))
                 0 sequence))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))


(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define (count-leaves1 x)
  (accumlate



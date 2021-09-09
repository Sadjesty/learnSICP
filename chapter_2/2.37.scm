;Realization of  `map` with multiple arguments
(define (map proc items . others)
  (define (simple-map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (simple-map proc (cdr items)))))
  (define (map-helper proc items others)
    (if (null? items)
        nil
        (cons (accumulate proc (car items) (simple-map car others))
                    (map-helper proc (cdr items) (simple-map cdr others)))))
  (if (null? others)
      (simple-map proc items)     
      (map-helper proc items others)))

;(map square (list 1 2 3))
;(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
;(map (lambda (x y) (+ (square x) (square y))) (list 1 2 3 4) (list 6 7 8 9))
  
(define a-vector (list 3 4 2 1))
(define b-vector (list 3 1 8 1))
(define a-matrix (list 
                  (list 2 4 0)
                  (list -2 1 3)
                  (list -1 0 1)))
(define c-vector (list 1 2 -1))
(define m-matrix (list
                  (list 3 4)
                  (list 2 1)))
(define n-matrix (list
                  (list 3 2)
                  (list 4 1)))
  
(define (dot-product v w) 
  (accumulate + 0 (map * v w)))

(define (transpose mat)
  (accumulate-n cons nil mat))
(transpose a-matrix)

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
         m))
(matrix-*-vector a-matrix c-vector)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector m row))
         cols)))
(matrix-*-matrix m-matrix n-matrix)

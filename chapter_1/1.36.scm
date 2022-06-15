(define (cont-frac n d k)
    (let ((n-val (n k))
           (d-val (d k)))
        (if (= k 0)
            (+ d-val (/ n-val d-val))
              (/ n-val
                (+ d-val (cont-frac n d (- k 1)))))))
(define (cont-frac-iter n d k)
    (define (iter i)
        (let ((n-val (n i))
               (d-val (d i)))
            (if (= i k)
                (+ d-val (/ n-val d-val))
                  (/ n-val
                    (+ d-val (iter (+ i 1)))))))
      (iter 0))

; 1.6180339887498948482
(/ 1.0 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11))
(/ 1.0 (cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           11))
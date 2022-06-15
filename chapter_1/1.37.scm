(define (cont-frac n d k)
    (let ((n-val (n k))
           (d-val (d k)))
        (if (= k 0)
            (+ d-val (/ n-val d-val))
              (/ n-val
                (+ d-val (cont-frac n d (- k 1)))))))

;1.6180339887498948482
(/ 1.0 (cont-frac (lambda (i) 1.0)
         (lambda (i) 1.0)
         11))
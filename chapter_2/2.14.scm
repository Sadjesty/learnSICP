
(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
      (add-interval r1 r2)))
(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
          (add-interval (div-interval one r1)
            (div-interval one r2)))))
(define A (make-interval 3.35 3.65))
(define B (make-interval 6.35 6.65))

(define (print-interval i)
    (newline)
      (print "(")
      (print (lower-bound i))
      (print " ")
      (print (upper-bound i))
      (print ")"))

(define (print-interval-percent i)
    (newline)
      (print "(")
      (print (center i))
      (print " +/- ")
      (print (percent i))
      (print "%)"))


(print-interval-percent (par1 A B))
(print-interval-percent (par2 A B))
(print-interval-percent (par1 A A))
(print-interval-percent (par2 A A))

"("2.2838054248823942" +/- "9.567985099156354"%)"
"("2.2747973175858274" +/- "3.593994060059396"%)"
"("1.7628808014720914" +/- "12.794514192931079"%)"
"("1.75" +/- "4.285714285714289"%)"
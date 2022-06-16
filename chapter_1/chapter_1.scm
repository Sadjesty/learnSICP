#lang scheme
;486

;(+ 137 349)
;(- 1000 334)
;(* 5 99)
;(/ 10 5)
;(+ 2.7 10)

;(+ 21 35 12 7)
;(* 25 4 12)

;(+ (* 3 5) (- 10 6))
;(+ (* 3
;      (+ (* 2 4)
;         (+ 3 5)))
;   (+ (- 10 7)
;      6))

(define size 2)
;(* size 2)

(define pi 3.14159)
(define radius 10)
;(* pi (* radius radius))
(define circumference (* 2 pi radius))
;circumference

(define (square x) (* x x))
;(square 21)
(define (sum-of-squares x y)
    (+ (square x) (square y)))
;(sum-of-squares 3 4)
(define (f a)
    (sum-of-squares (+ a 1) (+ a 2)))
;(f 5)

(define (abs x)
    (cond ((< x 0) (- x))
      (else x)))

(define (sum-of-two-bigger-numbers a b c)
    (if (> a b)
        (if (> b c)
            (sum-of-squares a b)
              (sum-of-squares a c))
          (if (> a c)
              (sum-of-squares a b)
                (sum-of-squares b c))))


(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
          (sqrt-iter (improve guess x)
            x)))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
    (sqrt-iter 1.0 x))


;1.2


(define (factorial n)
    (if (= n 1)
        1
          (* n (factorial (- n 1)))))
(define (factorial-i n)
    (define (fact-iter product counter max-count)
        (if (> counter max-count)
            product
              (fact-iter (* product counter)
                (+ counter 1)
                max-count)))
      (fact-iter 1 1 n))

(define (smallest-divisor n)
    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
      (define (divides? a b)
          (= (remainder b a) 0))
      (define (next a)
          (if (= a 2)
              3
                (+ a 2)))
      (find-divisor n 2))
(define (prime? n)
    (= n (smallest-divisor n)))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
      ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
          m))
      (else
        (remainder (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
    (cond ((= times 0) true)
      ((fermat-test n) (fast-prime? n (- times 1)))
      (else false)))

(define (runtime)
    (current-milliseconds))
(define (timed-prime-test n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (fast-prime? n 10000)
        (report-prime n (- (runtime) start-time))
          false))
(define (report-prime n elapsed-time)
    (display n)
      (display " *** ")
      (display elapsed-time)
      (display "\n"))

(define (get-all-primes-in-range a b)
    (timed-prime-test a)
      (if (= a b)
          (display "END\n")
            (get-all-primes-in-range (+ a 1) b)))



;1.3

(define (cube x) (* x x x))

;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b))))

;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (+ a 1) b))))

;(define (pi-sum a b)
;  (if (> a b)
;     0
;      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
    (if (> a b)
        0
          (+ (term a)
            (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
    (sum cube a inc b))
;(sum-cubes 1 10)

(define (idenity x) x)

(define (sum-integers a b)
    (sum identity a inc b))
;(sum-integers 1 10)

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
      (define (pi-next x)
          (+ x 4))
      (sum pi-term a pi-next b))
;(* 8 (pi-sum 1 1000000))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
      (* (sum f (+ a (/ dx 2)) add-dx b)
        dx))
;(integral cube 0 1 0.01)
;(integral cube 0 1 0.001)
;(integral cube 0 1 0.000001)

(define (simpson f a b n)
    (define h (/ (- b a) n))
      (define (y k) (f (+ a (* k h))))
      (define (simpson-term k)
          (cond ((= k 0) (y 0))
            ((= k n) (y n))
            ((even? k) (* 2 (y k)))
            (else (* 4 (y k)))))
      (* (/ h 3) (sum simpson-term 0 inc n)))
;(integral cube 0 1 0.01)
;(simpson cube 0 1.0 100)
;(integral cube 0 1 0.001)
;(simpson cube 0 1.0 1000)


(define (sum-it term a next b)
    (define (iter a result)
        (if (> a b)
            result
              (iter (next a) (+ (term a) result))))
      (iter a 0))


(define (product term a next b)
    (if (> a b)
        1
          (* (term a)
            (product term (next a) next b))))

(define (factorial-new n)
    (product identity 1 inc n))
;(factorial-new 5)
(define (pi-divided-by-four n)
    (define (term a)
        (/ (* (- a 1.0) (+ a 1.0))
          (square a)))
      (define (next a)
          (+ a 2))
      (product term 3.0 next n))
;(* (pi-divided-by-four 1000000) 4)

(define (product-it term a next b)
    (define (iter a result)
        (if (> a b)
            result
              (iter (next a) (* (term a) result))))
      (iter a 1))


(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
          (combiner (term a)
            (accumulate combiner null-value term (next a) next b))))

(define (accumulate-it combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
              (iter (next a) (combiner (term a) result))))
      (iter a null-value))


(define (filtered-accumulate filter combiner null-value term a next b)
    (cond ((> a b) null-value)
      ((filter a) (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b)))
      (else (filtered-accumulate filter combiner null-value term (next a) next b))))
(define (sum-of-prime a b)
    (filtered-accumulate prime? + 0 identity a inc b))
;(sum-of-prime 0 10)

(define (f1 x y)
    (let ((a (+ 1 (* x y)))
           (b (- 1 y)))
        (+ (* x (square a))
          (* y b)
          (* a b))))

(define (f2 g)
    (g 2))
;(f2 square)
;(f2 f2)


;1.3.3
(define (search f neg-point pos-point)
    (define (close-enough? x y)
        (< (abs (- x y)) 0.001))
      (let ((midpoint (average neg-point pos-point)))
          (if (close-enough? neg-point pos-point)
              midpoint
                (let ((test-value (f midpoint)))
                    (cond ((positive? test-value)
                            (search f neg-point midpoint))
                      ((negative? test-value)
                        (search f midpoint pos-point))
                      (else midpoint))))))

(define (half-interval-method f a b)
    (let ((a-value (f a))
           (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
                (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else
            (error "У аргументов не разные знаки " a b)))))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
      (define (close-enough? v1 v2)
          (< (abs (- v1 v2)) tolerance))
      (define (try guess)
          (let ((next (f guess)))
              (if (close-enough? guess next)
                  next
                    (try next))))
      (try first-guess))

(define golden-ratio
    (fixed-point (lambda (x) (+ 1 (/ 1 x)))
      1.0))
;golden-ratio

(define (fixed-point-printed f first-guess)
    (define tolerance 0.00001)
      (define (close-enough? v1 v2)
          (< (abs (- v1 v2)) tolerance))
      (define (try guess)
          (let ((next (f guess)))
              (newline)
                (print next)
                (if (close-enough? guess next)
                    next
                      (try next))))
      (try first-guess))

(define (solution-1.36 x)
    (fixed-point-printed (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (solution-1.36-average x)
    (fixed-point-printed (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

;(solution-1.36 10)
;(newline)
;(solution-1.36-average 10)

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

;1.6180339887498948482
;(/ 1.0 (cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           11))
;(/ 1.0 (cont-frac-iter (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           11))

;(cont-frac-iter (lambda (i) 1.0)
;  (lambda (i)
;      (cond ((= i 0) 1)
;        ((= i 1) 2)
;        ((= (remainder i 3) 1) (- i (- (/ i 3) 1)))
;        (else 1)))
;  10)



;1.3.4

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (deriv g)
    (define dx 0.00001)
      (lambda (x)
          (/ (- (g (+ x dx)) (g x))
            dx)))


(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))
(define (new-sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y))
      average-damp
      1.0))


(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;(newtons-method (cubic 0 0 -8) 1)

(define (double f)
    (lambda (x) (f (f x))))
;(((double (double double)) inc) 5)

(define (compose f g)
    (lambda (x) (f (g x))))
;((compose square inc) 6)

(define (repeated f times)
    (if (= times 1)
        f
          (compose f (repeated f (- times 1)))))
;((repeated square 2) 5)

(define (smooth f)
    (define dx 0.00001)
      (lambda (x) (/ (+ (f (- x dx))
                       (f x)
                       (f (+ x dx)))
                    3)))

(define (smooth-n f n)
    ((repeated (lambda (x) (smooth x)) n) f))
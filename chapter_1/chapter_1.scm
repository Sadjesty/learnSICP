#lang scheme
486

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

(+ 21 35 12 7)
(* 25 4 12)

(+ (* 3 5) (- 10 6))
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

(define size 2)
(* size 2)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

(define (square x) (* x x))
(square 21)
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
(define (f a)
  (sum-of-squares (+ a 1) (+ a 2)))
(f 5)

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))
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

(define (improve-cube guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))
(define (cube-iter guess x)
  (define new-guess (improve-cube guess x))
  (if (alt-good-enough? guess new-guess)
      guess
      (cube-iter new-guess x)))
(define (cube-root x)
  (cube-iter 1.0 x))

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


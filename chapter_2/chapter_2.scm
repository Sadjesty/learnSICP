#lang scheme

(define (abs x)
    (cond ((< x 0) (- x))
      (else x)))

(define (gcd a b)
    (if (= b 0)
        a
          (gcd b (remainder a b))))

(define (make-rat n d)
    (let ((g (abs (gcd n d))))
        (cond ((and (> n 0) (< d 0)) (cons (- (/ n g)) (abs (/ d g))))
          ((and (< n 0) (< d 0)) (cons (abs (/ n g)) (abs (/ d g))))
          (else (cons (/ n g) (/ d g))))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                (* (numer y) (denom x)))
      (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                (* (numer y) (denom x)))
      (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
      (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
      (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
      (* (numer y) (denom x))))

(define (print-rat x)
    (newline)
      (display (numer x))
      (display "/")
      (display (denom x)))

(define one-half (make-rat 1 2))

;(print-rat one-half)

(define one-third (make-rat 1 3))

;(print-rat (add-rat one-half one-third))
;(print-rat (mul-rat one-half one-third))
;(print-rat (add-rat one-third one-third))

;(print-rat (make-rat 1 3))
;(print-rat (make-rat -1 3))
;(print-rat (make-rat 1 -3))
;(print-rat (make-rat -1 -3))

(define (make-point x y)
    (cons x y))
(define (x-point a)
    (car a))
(define (y-point a)
    (cdr a))
(define (make-segment a b)
    (cons a b))
(define (start-segment s)
    (car s))
(define (end-segment s)
    (cdr s))
(define (print-point p)
    (newline)
      (display "(")
      (display (x-point p))
      (display ",")
      (display (y-point p))
      (display ")"))
(define (mid-point s)
    (let ((a (start-segment s))
           (b (end-segment s)))
        (make-point (/ (- (x-point b) (x-point a)) 2)
          (/ (- (y-point b) (y-point a)) 2))))
;(define A (make-point 1 2))
;(define B (make-point 5 6))
;(define AB (make-segment A B))

;(define mp (mid-point AB))
;(print-point mp)
(define (square x) (* x x))
(define (fast-expt b n)
    (define (fast-expt-iter a b n)
        (if (= n 0)
            a
              (if (even? n)
                  (fast-expt-iter a (square b) (/ n 2))
                    (fast-expt-iter (* a b) b (- n 1)))))
      (fast-expt-iter 1 b n))

(define (my-cons x y)
    (* (fast-expt 2 x)
      (fast-expt 3 y)))
(define (my-car p)
    (if (not (= (remainder p 2) 0))
        0
          (+ (my-car (/ p 2)) 1)))
(define (my-cdr p)
    (if (not (= (remainder p 3) 0))
        0
          (+ (my-cdr (/ p 3)) 1)))
;(my-car (my-cons 4 2))
;(my-cdr (my-cons 3 6))




(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))
(add-1 zero)


(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
      (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
           (p2 (* (lower-bound x) (upper-bound y)))
           (p3 (* (upper-bound x) (lower-bound y)))
           (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
          (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
      (make-interval (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
      (- (upper-bound x) (lower-bound y))))

(define (cross-zero? r)
    (and (< (lower-bound r) 0)
      (> (upper-bound r) 0)))

(define (div-interval-v2 x y)
    (let (divisor (make-interval (/ 1.0 (upper-bound y))))
        (if (cross-zero? divisor)
            (error "Dividing by zero")
              (mul-interval x divisor))))

(define (mul-interval x y)
    (let ((l1 (lower-bound x))
          (u1 (upper-bound x))
          (l2 (lower-bound y))
          (u2 (upper-bound y)))
      (cond ((and (and (< l1 0)
                       (< u1 0))
                  (and (> l2 0)
                       (> u2 0)))
             (make-interval (* l1 u2)
                            (* l2 u2)))
            ((and (and (< l1 0)
                       (< u1 0))
                  (and (> l2 0)
                       (> u2 0)))
             (make-interval (* l1 u2)
                            (* l2 u2)))


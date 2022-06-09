(define (runtime)
  (current-milliseconds))
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
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


(get-all-primes-in-range 1000000 1000037)
(get-all-primes-in-range 10000000 10000103)
(get-all-primes-in-range 100000000 100000040)
(get-all-primes-in-range 1000000000 1000000021)
(get-all-primes-in-range 10000000000 10000000065)
(get-all-primes-in-range 100000000000 100000000057)
(get-all-primes-in-range 1000000000000 1000000000080)

1000003 *** 0
1000033 *** 0
1000037 *** 0
END
10000019 *** 1
10000079 *** 0
10000103 *** 0
END
100000007 *** 1
100000037 *** 1
100000039 *** 0
END
1000000007 *** 1
1000000009 *** 1
1000000021 *** 2
END
10000000019 *** 6
10000000033 *** 6
10000000061 *** 6
END
100000000003 *** 19
100000000019 *** 20
100000000057 *** 25
END
1000000000039 *** 68
1000000000061 *** 93
1000000000063 *** 73
END
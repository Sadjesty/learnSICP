(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (sine 4.05))
(p (p (sine 1.34999)))
(p (p (p (sine 0.44999)))
(p (p (p (p (sine 0.14999)))))
(p (p (p (p (p (sine 0.04999))))))
(p (p (p (p (p 0.04999)))))

p will be invoked 5 times

Speed -> O(log(n))
Memory -> O(log(n))
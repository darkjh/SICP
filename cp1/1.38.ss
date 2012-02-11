;; Ex. 1.38
(define (cont-frac n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (calculate (+ 1 i))))))
  (calculate 1))

(define (euler k)
  (define (d i)
    (if (= (modulo i 3) 2)
        (* 2 (/ 3 (+ i 1)))
        1))
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))
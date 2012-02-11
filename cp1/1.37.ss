;; Ex. 1.37

(define (cont-frac n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (calculate (+ 1 i))))))
  (calculate 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))

  (iter k 0))
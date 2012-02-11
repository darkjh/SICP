;; Ex. 1.39
(define (cont-frac n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (calculate (+ 1 i))))))
  (calculate 1))

;; Define a method to calculate tan using Lambert formular
(define (tan-cf x k)
  (define (d i) (- (* i 2) 1))
  (define (n i)
    (if (= i 1) x
	(* -1 (* x x))))
  ;; body
  (cont-frac n d k))
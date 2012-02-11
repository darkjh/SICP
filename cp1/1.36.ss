;; Ex. 1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (show next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (show n)
  (newline)
  (display n))

;(fixed-point (lambda (x) (+ (/ x 2) (/ (log 1000) (log (* x x))))) 2.0)
;10 steps

;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;36 steps
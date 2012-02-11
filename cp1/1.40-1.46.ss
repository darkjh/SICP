;; Ex. 1.40 ~ 1.46

;; Fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Derivation
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; Average damp
(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

;; With all the functions above, we can define the Newton method
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

;; Another more abstract function
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
;; Then use it to define 2 sqrt functions
;; First one with fixed point
(define (sqrt_1 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1))
;; Another one with newton method
(define (sqrt_2 x)
  (fixed-point-of-transform (lambda (y) (- (* y y) x))
			    newton-transform
			    1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex. 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; Ex. 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; Ex. 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Ex. 1.43
(define (repeat f x)
  (if (= x 1)
      f
      (compose f (repeat f (- x 1)))))

;; Ex. 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f x)
		    (f (+ x dx))
		    (f (- x )))
		 3)))
(define (smooth-n f n)
  (repeat smooth n) f)

;; Ex. 1.45
;; A very powerful function that calculate arbitrary nth root of a positive number, implemented using high-order abstraiction
;; The variable i means the number of time needed for average-damp procedure to ensure the convergence
(define (nth-root x n)
  (let ((i (ceiling (/ (log n) (log 2)))))
    (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
			      (repeat average-damp i)
			      1)))

;; Ex. 1.46
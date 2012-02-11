;; Ex. 2.4

;; Here it is even less clear where the data is stored. my-cons returns a function that takes a function argument and calls it on its two own arguments. my-car provides it with a function that selects the first of its two arguments. Thinking like this, my-cdr is very simple

(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

;; Ex. 2.5

;; Another special form of cons, car and cdr

(define (my-cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (my-car z)
  (if (= (remainder z 2) 1)
      0
      (+ 1 (my-car (/ z 2)))))

(define (my-cdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (my-cdr (/ z 3)))
      0))

;; Ex. 2.6
;; We are able to define inc and zero without using any number at all.
;; It is called the Church numbering

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
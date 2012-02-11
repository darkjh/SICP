(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (factorial a b)
  (product identity a inc b))
(define (wallis a b)
  (define (term x)
    (/ (* (+ x 1) (- x 1)) (square x)))
  (define (next x)
    (+ x 2))
  (* 4 (product term (+ a 2) next b)))
  


(define (inc n) (+ n 1))
(define (indentity x) x)
(define (square x) (* x x))
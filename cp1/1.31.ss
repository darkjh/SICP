;; Ex. 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))


;; Using high-order function product for factoriel
(define (fac b)
  (define (inc a) (+ a 1))
  (define (term a) a)
  (product term 1 inc b))

;; Also using function product for pi
(define (pi precision)
  (define (inc-2 a) (+ a 2))
  (define (term n)
    (* (/ (- n 1) n)
       (/ (+ n 1) n)))
  (* 4 (product term 3 inc-2 precision)))
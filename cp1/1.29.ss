;; Ex. 1.29
;; Def. of the high-order abstraction function sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; Def. of integral function using a dx numeric method
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2)) add-dx b)
     dx))

;; Def. of integral function using Simpson Rule
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc k) (+ k 1))
  (define (next x) (+ x h))
  (define (even? x)
    (= (remainder x 2) 0))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (term k)
    (* (coef k)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum term 0 inc n)))

;; Ex. 1.30
;; Iter version of high-order function sum
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))
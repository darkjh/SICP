;; Ex. 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; Using this even-higher-order function to define sum
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

;; Again, for product
(define (product-acc term a next b)
  (accumulate * 1 term a next b))

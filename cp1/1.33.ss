;; Ex. 1.33
;; Adding a filter to accumulate
(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term (next a) next b predicate))
          (next a))))

(define (sum-of-odd a b)
  (filtered-accumulate + 0 identify a inc b odd?))
(define (identify n)
   n)
(define (inc a)
  (+ a 1))

(define (filtered-acc combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate a)
                  (combiner result (term a))
                  result))))
    (iter a null-value))
(define (gcd-new a b)
  (if (= b 0)
      a
      (gcd-new b (remainder a b))))
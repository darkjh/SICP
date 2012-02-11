(define (big a b c)
  (cond ((and (> a b) (> a c) (> b c)) (+ a b))
        (else (+ b c))
        ))
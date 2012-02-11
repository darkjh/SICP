(define (factorial n)
  (define (iter counter product)
    (if (> counter n)
        product
        (iter (+ 1 counter) (* product counter))))
  (iter 1 1)
  )

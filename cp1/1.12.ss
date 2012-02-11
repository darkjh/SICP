(define (pascal row col)
  (if (or (> col row) (< col 1) (< row 1))
      'error
      (triangle row col)))
(define (triangle row col)
  (if (or (= col 1) (= col row))
       1
      (+ (triangle (- row 1) (- col 1)) (triangle (- row 1) col))))

        
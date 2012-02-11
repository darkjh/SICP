(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  ;; Beginning of the iter part
  (define (iter p1 p2 p3 count)
    (if (= count 2)
	p3
	(iter p2 p3 (+ (* 3 p1) (* 2 p2) p3) (- count 1))))
  ;; Wrapper of the iter part
  (if (< n 3)
      n
      (iter 0 1 2 n)))
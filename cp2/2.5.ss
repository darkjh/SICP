;sicp 2.5
;利用过程来表示一个数对
(define (pow a n)
  (define (pow-iter a counter result)
    (if (= counter 0)
        result
        (pow-iter a (- counter 1) (* result a))))
  (pow-iter a n 1))

(define (cons1 x y)
  (* (pow 2 x)
     (pow 3 y)))

(define (car1 z)
  (if (= (remainder z 2) 1)
      0
      (+ 1 (car1 (/ z 2)))))
 
(define (cdr1 z)
  (if (= (remainder z 3) 0)
      (+ 1 (cdr1 (/ z 3)))
      0))
  
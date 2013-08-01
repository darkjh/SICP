;; 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

;; returns 3
(count-pairs (list 1 2 3))
;; returns 4
(define one (list 1))
(define four (cons 1 (cons a a)))
(count-pairs four)
;; returns 7
(define one (list 1))
(define three (cons one one))
(define seven (cons three three))
(count-pairs seven)

;; 3.17
(define (count-pairs x)
  (define (inner x memo-list)
    (if (and (pair? x)
             (not (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))
  (length (inner x '())))

;; 3.18
(define (circle? l)
  (define (circle-test memo-list p)
    (cond ((not (pair? p)) #f)
	  ((memq p memo-list) #t)
	  (else (circle-test (cons p memo-list) (cdr p)))))
  (circle-test '() l))

;; 3.19
;; using two ptrs, one goes one step each time, another goes two
;; if the two ptrs meet each other before reach the end of the list
;; then the list has cycle
(define (circle? l)
  (define (tail? p)
    (if (or (null? p) (null? (cdr p)))
	#t
	#f))
  (define (circle-test slow fast)
    (cond ((and (eq? slow fast))
	   #t)
	  ((or (tail? slow) (tail? fast))
	   #f)
	  (else
	   (circle-test (cdr slow) (cddr fast)))))
  (circle-test l (cdr l)))

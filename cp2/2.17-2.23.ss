;; Ex. 2.17 - 2.23

;; Implentation of some list operations
;; list-ref

(define (list-ref list n)
  (if (= n 0)
      (car list)
      (list-ref (cdr list) (- n 1))))
;; append
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))

;; Ex. 2.17

(define (last-pair1 z)
  (if (null? (cdr z))
      z
      (last-pair1 (cdr z))))

;; Ex. 2.18

(define (reverse1 items)
  (define (rev items result)
    (if (null? items)
        result
        (rev (cdr items) (cons (car items) result))))
  (rev items (list)))

;; Ex. 2.20
;; The formal parameter after the point can receive several actual parameter and stock them in a list
(define (same-parity x . y)
  (define (sp x y result)
    (if (null? y)
        result
        (if (= 0 (remainder (- (car y) x) 2))
            (sp x (cdr y) (cons (car y) result))
            (sp x (cdr y) result))))
  (sp x y (list)))

;; Ex 2.21

(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items))
            (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))

;; Ex. 2.21
;; le mot cle 'begin' sert a executer la structure sequentielle
(define (for-each1 fonc items)
  (if (null? items)
      null
      (begin (fonc (car items))
	     (for-each1 fonc (cdr items)))))

(define (for-each2 f l)
  (cond ((null? l) #t)
        (else (f (car l))
	      (for-each2 f (cdr l)))))

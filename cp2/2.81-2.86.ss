;; Ex. 2.81 - 2.86

;; Ex. 2.83
;; 类型提升，raise操作
;; 思路是在每个类型的程序包里添加往自己上一层转型的raise操作，并且在操作-类型表格中加入这个操作。调用的时候只需要apply-generic就可以了

;; Into integer package
(define (integer->rational n)
  (make-rational n 1))

(put 'raise '(integer)
     (lambda (i) (integer->rational i)))

;; Into rational package
(define (rational->real r)
  (make-real
   (exact->inexact
    (/ (numer r) (denom r)))))

(put 'raise '(rational)
     (lambda (r) (rational->real r)))

;; Into real package
(define (real->complex r)
  (make-complex-from-real-imag r 0))

(put 'raise '(real)
     (lambda (r) (real->complex r)))

(define (raise x)
  (apply-generic 'raise x))


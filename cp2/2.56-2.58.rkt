;; Ex. 2.56 - 2.58
;; 一个简单的符号求导程序

;; 第一层抽象
;; 底层函数的实现以及代数式的表达

(define (var? x) (symbol? x))
(define (same-var? v1 v2)
  (and (var? v1) (var? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Ex. 2.56
;; 增加对exp求导的支持

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
        (else (list '^ e1 e2))))

;; 第二次抽象
;; deriv函数把上面的过程当作基本过程

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp)
         (if (same-var? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
							  (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression!"))))




;; Ex. 2.57
;; 改动augend和multiplicand函数，使得乘法和加法能处理两个以上操作数

(define (augend s)
  (let ((t (cddr s)))
    (if (pair? (cdr t))
        (cons '+ t)
        (car t))))

(define (multiplicand p)
  (let ((t (cddr p)))
    (if (pair? (cdr t))
        (cons '* t)
        (car t))))

;; Ex. 2.58


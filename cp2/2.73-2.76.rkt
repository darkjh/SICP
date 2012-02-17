;; Ex. 2.73 - 2.76
;; 数据导向的程序设计

;; 数据表示

;; 全局的操作-类型对应矩阵
(define global-array '())
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

;; 实现数据导向设计的两个必要函数，put和get，用于存储和取得和数据类型相匹配的操作
(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) false)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一个小例子，对于复数计算，可以存在两种不同的表示方式：实部虚部和极坐标。这里用数据导向的设计方法构建出一个纵向的抽象屏障，隔离两种不同的表示方式。
;; 数据导向的设计方式满足程序的可加性，使得模块化构建更加容易

;; 实部虚部表示的程序包
(define (install-rectangular-package)
  ;; 基本过程
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (square x) (* x x))

  ;; 给上述过程加上标记，放入操作-类型矩阵中
  (define (mytag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'angle '(rectangular) angle)
  (put 'magnitude '(rectangular) magnitude)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (mytag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (mytag (make-from-mag-ang r a))))

  'done)

;; 极坐标表示的程序包
(define (install-polar-package)
  ;; 基本过程
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (square x) (* x x))

  ;; 给上述过程加上标记，放入矩阵
  (define (mytag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (mytag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (mytag (make-from-mag-ang r a))))

  'done)


;; 加标记
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;; 检测标记
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
;; 取出内容（函数体）
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;; 检测是否是实部虚部表示
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
;; 检测是否是极坐标表示
(define (polar? z)
  (eq? (type-tag z) 'polar))


;; 数据导向的选择函数，可以把上面的程序包看作是构造函数
;; apply-generic 把和args数据类型对应的op映射到args上去
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;; 用apply-generic来定义通用型的复数计算操作
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; 使用复数基本操作的抽象层

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; Ex. 2.73
;; 几个谓词函数
(define (same-variable v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (variable? x) (symbol? x))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; 求导函数包
(define (install-deriv-package)
  ;; 内部过程
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (devir (multiplier exp) var)
                   (multiplicand exp))))
  ;; 放入矩阵，作为界面
  (put '(deriv) '+ deriv-sum)
  (put '(deriv) '* deriv-product)

  'done)

;; 调用函数
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;; Ex. 2.75
;; 使用消息传递设计策略设计的构造函数make-from-mag-ang

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) mag)
	  ((eq? op 'angle) ang)
	  ((eq? op 'real-part)
	   (* mag (cos ang))
	  ((eq? op 'imag-part)
	   (* mag (sin ang))))))
  dispatch)
;; System for calculating intervals

;; Ex. 2.7
;; Representation of intervals
(define (make-interval a b)
  (cons (min a b) (max a b)))
(define (upper-bound z)
  (cdr z))
(define (lower-bound z)
  (car z))

;; Ex. 2.8
;; Four calculas procedure of intervals
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (upper-bound x))
                 (- (upper-bound y) (lower-bound x))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
      (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

;; Ex. 2.10
;; Avoid intervals that are across 0 when dividing

(define (div-interval-check x y)
  (define (ok? a)
    (if (<= (* (upper-bound a) (lower-bound a)) 0)
	#f
	#t))
  (if (ok? y)
      (div-interval x y)
      'error))

;; Ex 2.11
;; Divide multiplication into 9 cases

;   xl xu yl yu  Min              Max
;   ---------------------------------------
;1.  +  +  +  +   (* xl yl)        (* xu yu)
;2.  +  +  +  -   Invalid          Invalid
;3.  +  +  -  +   (* xl yl)        (* xu yu)
;4.  +  +  -  -   (* xu yu)        (* xl yl)
;5.  +  -  +  +   Invalid          Invalid
;6.  +  -  +  -   Invalid          Invalid
;7.  +  -  -  +   Invalid          Invalid
;8.  +  -  -  -   Invalid          Invalid
;9.  -  +  +  +   (* xl yu)        (* xu yu)
;10. -  +  +  -   Invalid          Invalid
;11. -  +  -  +   (min             (* xu yu)
;                  (* xl yu)
;                  (* xu yl)
;12. -  +  -  -   (* xu yu)        (* xl yu)
;13. -  -  +  +   (* xu yu)        (* xl yl)
;14. -  -  +  -   Invalid          Invalid
;15. -  -  -  +   (* xu yu)        (* xu yl)
;16. -  -  -  -   (* xl yl)        (* xu yu)
;   -----------------------------------------
; Totally 9 valid cases - only 1 requires more than 2 multiplications (i.e. 11. above)

(define (mul-interval x y)
  (define (sign f a)
    (positive? (f a)))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y))
        (pxl (sign lower-bound x))
        (pxu (sign upper-bound x))
        (pyl (sign lower-bound y))
        (pyu (sign upper-bound y)))
        (cond ((and pxl pxu pyl pyu) (make-interval (* xl yl) (* xu yu))) ;1
              ((and pxl pxu (not pyl) pyu) (make-interval (* xl yl)(* xu yu))) ;3
              ((and pxl pxu (not pyl) (not pyu)) (make-interval (* xu yu)(* xl yl))) ;4
              ((and (not pxl) pxu pyl pyu) (make-interval (* xl yu)(* xu yu))) ;9
              ((and (not pxl) pxu (not pyl) pyu) (make-interval (min (* xl yu) (* xu yl)) (* xu yu))) ;11
              ((and (not pxl) pxu (not pyl) (not pyu)) (make-interval (* xu yu) (* xl yu))) ;12
              ((and (not pxl) (not pxu) pyl pyu) (make-interval (* xu yu) (* xu yl))) ;13
              ((and (not pxl) (not pxu) (not pyl) pyu) (make-interval (* xu yu) (* xu yl))) ;15
              ((and (not pxl) (not pxu) (not pyl) (not pyu)) (make-interval (* xl yl) (* xu yu))) ;16
              (else (error "Invalid interval"))
              )))


;; Another set of construction and selection functions
;; ex. 3.5 +- 0.15

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Ex. 2.12
;; Construct an interval by a center value and a error percentage
(define (make-center-percent c p)
  (let ((i (/ (* c p) 100)))
          (make-interval (- c i) (+ c i))))
;; The according selection functions
(define (percent i)
  (* (/ (width i) (center i)) 100))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; Ex. 2.14

(define (parl r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (parl2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))









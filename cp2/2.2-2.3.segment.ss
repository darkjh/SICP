;; Ex. 2.2

(define (make-segment a b)
  (cons a b))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (make-points x y)
  (cons x y))
(define (x-point x)
  (car x))
(define (y-point x)
  (cdr x))

(define (midpoint-segment seg)
  (define (average a b) (/ (+ a b) 2))
  (let ((s (start-segment seg)) (e (end-segment seg)))
    (make-points (average (x-point s) (x-point e))
		 (average (y-point s) (y-point e)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display " , ")
  (display (y-point p))
  (display ")")
  (newline))

;; Ex. 2.3

;; If we have the abstraict interface of rectagle, the functions who deal with rectangles can be defined without any knowledge of the representation of rectangle itself
;; Interface
;; (rect a b) -> defines a rectangle
;; (width-rect rect) -> returns the width
;; (length-rect rect) -> returns the length

;; So here goes the functions that calculate things like perimeter or surface
(define (perimeter-rect rect)
  (+ (* 2 (width-rect rect))
     (* 2 (length-rect rect))))
(define (area-rect rect)
  (* (width-rect rect)
     (length-rect rect)))

;; After the abstraict interface, we define the representation of rectangles, it can be in various forms

;; Def. rectangle
(define (rect x y)
  (cons x y))
(define (width-rect rect)
  (let ((w (abs (- (x-point (car rect))
		   (x-point (cdr rect)))))
	(l (abs (- (y-point (car rect))
		   (y-point (cdr rect))))))
    (if (< w l)
	w
	l)))
(define (length-rect rect)
    (let ((w (abs (- (x-point (car rect))
		   (x-point (cdr rect)))))
	(l (abs (- (y-point (car rect))
		   (y-point (cdr rect))))))
    (if (< w l)
	l
	w)))
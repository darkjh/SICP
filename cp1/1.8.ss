(define (func guess x)
  (if (good-enough? guess x)
      guess
      (func (improve guess x)
            x)))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.001))
(define (square x)
  (* x x))
(define (sqrt3 x)
  (func 1.0 x))
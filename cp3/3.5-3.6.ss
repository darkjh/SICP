;; 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
	  ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral trials p x1 x2 y1 y2)
  (let ((surface (* (abs (- x1 x2))
		    (abs (- y1 y2)))))
    (* surface (monte-carlo trials p))))

(define (square x)
  (* x x))

;; unit circle centered at (5.0, 7.0)
(define (unit-circle-test)
  (let ((x (random-in-range 4.0 6.0))
	(y (random-in-range 6.0 8.0)))
    (<= (+ (square (- x 5.0)) (square (- y 7.0))) 1.0)))

;; 3.6
;; need to find a Scheme implementation with rand-update function

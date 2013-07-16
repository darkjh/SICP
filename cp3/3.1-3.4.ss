;; 3.1
(define (make-accumulator init)
  (lambda (amount)
    (begin (set! init (+ init amount))
	   init)))

;; 3.2
(define (make-monitored f)
  (let ((times 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) times)
	    ((eq? m 'reset-count) (set! times 0))
	    (else (begin (set! times (+ 1 times))
			 (f m)))))
    mf))

;; 3.3
;; bank account with password
(define (make-account balance code)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (dispatch c m)
    (cond ((not (eq? c code)) "Incorrect password")
	  ((eq? m 'withdraw) withdraw)
	  (else (error "Unknown request" m))))
  dispatch)

;; 3.4
;; enter wrong password for 7 times then call the police
(define (make-account balance code)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (call-the-police)
    "Police !!!")
  (let ((count 0))
    (define (dispatch c m)
      (cond ((eq? count 7) (call-the-police))
	    ((not (eq? c code))
	     (begin
	       (set! count (+ count 1))
	       "Incorrect password"))
	    ((eq? m 'withdraw) withdraw)
	    (else (error "Unknown request" m))))
    dispatch))

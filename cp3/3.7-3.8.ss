;; 3.7
(define (make-account balance code)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	(error "withdraw" "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  o(define (dispatch c m)
    (cond ((not (eq? c code))
	   (error "dispatch" "Incorrect password"))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "dispatch" "Unknown request"))))

  dispatch)

;; create another entrance for a password protected account
(define (make-joint acc acc-code new-code)
  (define (dispatch c m)
    (cond ((not (eq? c new-code))
	   (error "dispatch" "Incorrect password"))
	  ((eq? m 'withdraw) (acc acc-code 'withdraw))
	  ((eq? m 'deposit) (acc acc-code 'deposit))
	  (else (error "disptach" "Unknown reaquest"))))
  dispatch)

;; 3.8
(define f
  (let ((magic #t))
    (define (t n)
      (if magic
	  (begin (set! magic #f) n)
	  0))
    t))

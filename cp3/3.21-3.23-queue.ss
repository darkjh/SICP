;; A queue implementation
;; using two pointers and a mutable list
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      "Front call on empty queue"
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (begin
	     (set-front-ptr! queue new-pair)
	     (set-rear-ptr! queue new-pair)
	     queue))
	  (else
	   (begin
	     (set-cdr! (rear-ptr queue) new-pair)
	     (set-rear-ptr! queue new-pair)
	     queue)))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	 "Delete on empty queue")
	(else
	 (begin
	   (set-front-ptr! queue (cdr (front-ptr queue)))
	   queue))))

;; 3.21
;; just print the mutable list
(define (print-queue queue)
  (display (front-ptr queue)))

;; 3.22
;; implemente a queue by maintaining a mutable inner env
;; a little bit of OO flavor
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty?)
	  "Front call on empty queue"
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty?)
	       (begin
		 (set! front-ptr new-pair)
		 (set! rear-ptr new-pair)
		 front-ptr))
	      (else
	       (begin
		 (set-cdr! rear-ptr new-pair)
		 (set! rear-ptr new-pair)
		 front-ptr)))))
    (define (delete-queue!)
      (cond ((empty?)
	     "Delete on empty queue")
	    (else
	     (begin
	       (set! front-ptr (cdr front-ptr))
	       front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
	    ((eq? m 'empty-queue?) empty?)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    (else "Undefined operation")))
    dispatch))

;; 3.23
;; a deque implementation
;; need to be backed by a double linked list for O(1) ops
(define (make-deque)
  ;; make a double linked list cell
  (define (make-list-cell item prev next)
    (cons (cons item prev) next))
  (define (prev cell) (cdar cell))
  (define (next cell) (cdr cell))
  (define (item cell) (caar cell))
  (define (set-prev! cell ptr)
    (set-cdr! (car cell) ptr))
  (define (set-next! cell ptr)
    (set-cdr! cell ptr))

  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
	  "Front call on empty deque"
	  (item front-ptr)))
    (define (rear)
      (if (empty?)
	  "Rear call on empty deque"
	  (item rear-ptr)))
    (define (front-insert i)
      (cond ((empty?)
	     (let ((new-cell
		   (make-list-cell i '() '())))
	       (begin
		 (set! front-ptr new-cell)
		 (set! rear-ptr new-cell)
		 front-ptr)))
	    (else
	     (let ((new-cell
		   (make-list-cell i '() front-ptr)))
	       (begin
		 (set-prev! front-ptr new-cell)
		 (set! front-ptr new-cell)
		 front-ptr)))))
    (define (rear-insert i)
      (cond ((empty?)
	     (let ((new-cell
		    (make-list-cell i '() '())))
	       (begin
		 (set! front-ptr new-cell)
		 (set! rear-ptr new-cell)
		 front-ptr)))
	    (else
	     (let ((new-cell
		    (make-list-cell i rear-ptr '())))
	       (begin
		 (set-next! rear-ptr new-cell)
		 (set! rear-ptr new-cell)
		 front-ptr)))))
    (define (front-delete)
      (cond ((empty?)
	     "Front delete call on empty deque")
	    ((null? (cdr front-ptr))
	     ;; only one item
	     (begin
	       (set! front-ptr '())
	       (set! rear-ptr '())
	       front-ptr))
	    (else
	     (begin
	       (set! front-ptr (next front-ptr))
	       (set-prev! front-ptr '())
	       front-ptr))))
    (define (rear-delete)
      (cond ((empty?)
	     "Rear delete call on empty deque")
	    ((null? (cdr front-ptr))
	     ;; only one item
	     (begin
	       (set! front-ptr '())
	       (set! rear-ptr '())
	       front-ptr))
	    (else
	     (begin
	       (set! rear-ptr (prev rear-ptr))
	       (set-next! rear-ptr '())
	       front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) empty?)
	    ((eq? m 'front-deque) front)
	    ((eq? m 'rear-deque) rear)
	    ((eq? m 'front-insert-deque!) front-insert)
	    ((eq? m 'rear-insert-deque!) rear-insert)
	    ((eq? m 'front-delete-deque!) front-delete)
	    ((eq? m 'rear-delete-deque!) rear-delete)
	    (else "Undefined operation")))
    dispatch))

;; Unit testing for deque
(define dq (make-deque))
((dq 'empty-deque?))

((dq 'front-insert-deque!) 2)
((dq 'front-insert-deque!) 1)
((dq 'rear-insert-deque!) 3)
((dq 'rear-insert-deque!) 4)

((dq 'front-delete-deque!))
((dq 'front-delete-deque!))
((dq 'rear-delete-deque!))
((dq 'rear-delete-deque!))

((dq 'empty-deque?))

((dq 'rear-delete-deque!))

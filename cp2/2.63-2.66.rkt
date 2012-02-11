
;tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;2.65

;use the function to convert tree to list
;then union or intersection with list functions
;after that convert the result to tree
;order: O(3n)

;2.66
(define (lookup-bintree key set)
  (cond ((null? set) '())
        ((= key (entry set)) (entry set))
        ((< key (entry set)) (lookup-bintree key (left-branch set)))
        ((> key (entry set)) (lookup-bintree key (right-branch set)))))


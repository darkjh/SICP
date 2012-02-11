;; Ex. 2.24 - 2.29

;; Count-leaves
;; The count of leaves is the sum of leaves of a car and that of a cdr
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; Test tree
(define x (list (list 1 2) (list 3 4)))

;; Ex. 2.27
(define (deep-reverse items)
  (define (rev items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else (rev (cdr items)
		     (cons (rev (car items) (list)
				) result)))))
  (rev items (list)))

;; Ex. 2.28
;; 一直走到树叶（非cons对），然后将其加入结果表中
;; 不然则分别对左子树和右子树做递归调用，不要忘了需要把他们分别返回的结果表链接起来（append）
(define (fringle tree)
  (define (frig tree result)
    (cond ((null? tree) result)
          ((not (pair? tree)) (cons tree result))
          (else (append (frig (car tree) null) (frig (cdr tree) null)))))
  (frig tree (list)))

;; Ex. 2.29
;; 构造函数
(define (make-mobile l r)
  (list l r))
(define (make-branch len struct)
  (list len struct))

;; 相应的选择函数
(define (left-branch z)
  (car z))
(define (right-branch z)
  (cadr z))

(define (branch-len z)
  (car z))
(define (branch-struct z)
  (cadr z))

;; 遍历树，求叶子节点weight之和
(define (total-weight branch)
  (if (not (pair? (branch-struct branch)))
      (branch-struct branch)
      (+ (total-weight (left-branch branch))
	 (total-weight (right-branch branch)))))

;; 检查一个活动体是否平衡
;; 一个节点平衡必须满足三个条件：如果有子活动体，那么左右子活动体必须平衡。如果没有子活动体，那么左分支和右分支的力矩必须相等
;; 所以对于每一个节点，在遍历过程中，先检查左右节点是否有子活动体，有的话就递归调用。如果没有，就检查力矩并返回true或者false
(define (balance-mobile? mobile)
  (define (has-sub? branch)
    (pair? (branch-struct branch)))
  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (if (has-sub? left)
	     (balance-mobile? (branch-struct left)))
	 (if (has-sub? right)
	     (balance-mobile? (branch-struct right)))
	 (= (* (branch-len left) (branch-struct right))
	    (* (branch-len right) (branch-struct right))))))

;; 最后一小题
;; 如果更改了构造函数，比如说由list变为了cons，那么只需要更改相应的选择函数就行了。数据结构的概念就是构造函数和选择函数，这两个概念提供了数据抽象。而工作在这个抽象之上的各类功能函数，是不需要知道底层细节的，所以只要我们通过选择函数提供一致的数据界面，那么抽象层之上的函数是不会被构造函数的变化所影响的


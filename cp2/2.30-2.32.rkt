;; Ex. 2.30 - 2.32

;; 对树的所有子树映射一个函数，这个函数把叶子节点的值放大
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Ex. 2.30
(define (square-tree tree)
  (map (lambda (sub)
         (if (pair? sub)
             (square-tree sub)
             (* sub sub)))
       tree))

;; Ex. 2.31
;; 把对子树映射函数这个过程抽象出来
;; 调用 (tree-map (lambda (x) (* x x)) some-tree)
(define (tree-map func tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map func (car tree))
                    (tree-map func (cdr tree))))))
;; Ex. 2.32
;; 这道题需要先确定递归表达式
;; 1 空集的子集为空集
;; 2 非空集合的子集为
;;   a - 所有不含有某个元素（这里是 （car s））的集合
;;   b - 所有含有这个元素的集合
;;   的并集
;; 下面的实现中，if的第一个分支，是条件1，第二个分支中的rest是a，map和lambda定义的函数则构建了b
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
			  rest)))))






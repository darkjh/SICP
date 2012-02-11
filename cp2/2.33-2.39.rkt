;; Ex. 2.33 - 2.39

;; 累积器
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))
;; 树枚举器
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; Ex. 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;; Ex. 2.34
(define (horner-eval x coef-sequence)
  (accumulate (lambda (this-coef higher-terms)
		(+ this-coef (* x higher-terms)))
              0
              coef-sequence))

;; Ex. 2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1))
              0
              (map enumerate-tree t)))

;; Ex. 2.36
;; 对于一个序列中的n个长度相等的子序列做某种运算
;; 例如 (accumulate-n + 0 (list (list 1 2 3) (list 3 4 5) (list 4 5 6)))
;; 利用accumulate函数，每一步使用map取出每个序列相应的元素交给op去操作

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Ex. 2.37
;; Test data
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 1 2 3)))
(define v (list 1 2 3 4))

;; 计算两个简单向量的点积，向量用list表示
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))
;; 利用点积，计算矩阵和一个向量的乘积（向量在这里默认都是列向量）
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
;; 计算矩阵和矩阵的乘积
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
;; 取对每个子序列相应位置的一个元素应用cons，构造转置矩阵
(define (transpose mat)
  (accumulate-n cons null mat))

;; Ex. 2.38
;; accumulate又叫做fold-right，因为他把序列的第一个元素，通过op组合到右边所有元素的组合结果上去，比如(accumulate / 1 (list 1 2 3))结果是3/2. 这是因为（f（1）/（f（2）/（f（3）））），而f（3）=3，则有f（2）/f（3）=2/3，再被一除就是3/2
;; 而fold-left则正相反。(fold-left / 1 (list 1 2 3))结果是1/6. 因为代换式变为了(f(1)/f(2))/f(3).
;; 如果要让这两个方向的运算结果相同，那么op必须满足 a op b = b op a 即交换律
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

;; Ex. 2.39
(define (reverse1 seq)
  (accumulate (lambda (x y) (append y (list x))) null seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) null seq))










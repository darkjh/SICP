;; Ex. 2.67 - 2.72
;; Huffman Tree

;; 哈夫曼树基本结构
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? obj)
  (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; 结合两个节点，生成新节点
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; 选择函数，对应上面的构造函数
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; 解码过程，即从一个0/1表翻译出一段符号信息

;; 思路是根据一个bit，在当前节点的两个分支里选择一个。然后观察这个分支的下一个节点是否是叶子节点，如果是，那么意味着这个bit代表了该叶子节点的符号，于是就把这个符号加入结果表，当前节点回到树根，而继续检查剩下的bit。如果选择出的分支的下一个节点不是叶子节点，那么就向前走一部，以下一个节点为当前节点，继续检查下一个bit
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


;; 将新节点有序的加入到符号集合中
;; 也能对一组一组的符号小集合进行操作
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; 构造初始数据，即符号和其出现频率
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;符号
                               (cadr pair)) ;出现频率
                    (make-leaf-set (cdr pairs))))))

;; Ex. 2.67
;; 手工构造一个哈夫曼树做试验

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'X 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Ex. 2.68
;; Encoding

;; encoding框架
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; 存在性检查函数
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; 对一个符号进行编码
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons '0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons '1 (encode-symbol symbol (right-branch tree))))))

;; Ex. 2.69
;; 生成huffman树用于编码和解码

;; 框架函数
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; 将最小的两个符号合并成一颗树，并作为单个复合符号加入到符号集合中
;; 利用从符号集合已排序的特性，每次合并前2个集合元素，并使用adjoin-set依照顺序把新合并的元素加入集合。直到集合中只有一个元素的时候停止，这个元素就是树根

(define (successive-merge set)
  (if (= 1 (length set))
      (car set)
      (successive-merge (adjoin-set
                         (make-code-tree (car set)
                                         (cadr set))
                         (cddr set)))))


;; Ex. 2.70
;; 小试验

(define sample-list
  (list (list 'a 2) (list 'na 16) (list 'boom 1) (list 'sha 3)
	(list 'get 2) (list 'yip 9) (list 'job 2) (list 'wah 1)))
(define test-tree (generate-huffman-tree sample-list))

;; Ex. 2.71



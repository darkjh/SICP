;; Ex 2.40 - 2.42

;; 数据抽象
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (enumerate-interval l h)
  (if (> l h)
      null
      (cons l (enumerate-interval (+ l 1) h))))

;; 生成所有i和j的序对，满足i<j<n
(define (generate-special-pairs n)
  (accumulate append
	      null
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

;; flatmap抽象出一个过程，这个过程对seq的每个元素map一个操作proc，然后将该操作返回的序列append起来。比如对于generate-special-pairs的实现，这个抽象出的过程体现在对于每个小于n的数j，map一个函数，其生成所有的(i,j)序对并满足i<j
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (myfilter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (myfilter predicate (cdr sequence))))
        (else (myfilter predicate (cdr sequence)))))

;; 一个求全排列的算法
;; 递归规则：对于序列S里的每一个x，递归的生成集合S-x的排列，然后在这些排列的前面加上x。这样就能对S中的每个x（每个元素），生成以x开头的全部排列。那么将所有这些以各个元素开头的排列组合起来，就是最终结果了
;; 简单的remove函数，用filter抽象来实现
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))
(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
;; Ex. 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Ex. 2.41
(define (ordered-triple n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))
(define (triple-sum pair)
  (+ (car pair)
     (cadr pair)
     (caddr pair)))
(define (ordered-triple-sum n s)
  (myfilter (lambda (i) (= (triple-sum i) s)) (ordered-triple n)))

;; Ex. 2.42
;; Queens

;; 提供的程序框架
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list '())
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;; 格局集合表示方式
;;   一个格局就是n个元素的一个list。每个元素代表行位置，而n是棋盘的大小。
;;   比如(1,2,3)就代表在一个3x3的棋盘里，第一列的皇后放在第一行,(1,1);第二列的皇后放在第二行(2,2);第三列的皇后放在第三行,(3,3)。这就是3x3棋盘皇后问题的一个解。
;;   很自然的，格局集合就是这些list的一个list
;;   为了方便起见，使用倒序的排法表示一个格局，参见adjoin-position

;; 空格局
(define (empty-board) '())

;; 将新格局加入结果集合，这里是逆向的，即第一个元素是第k列的皇后所在的行数
(define (adjoin-position new k rest)
  (cons new rest))

;; 检查一个格局对于现有的格局集合是否安全
;; 内部的check函数执行检查
;;   i用来对列标号;x是新加入的皇后所在的行数，k是其列数，这个是需要检查的皇后，seq是其他皇后所在的行

(define (safe? k positions)
  (define (check i x k seq)
    (if (null? seq)
	#f
	(or (= x (car seq))
	    (= (abs (- k i))
	       (abs (- x (car seq))))
	    (check (- i 1) x k (cdr seq)))))

  (if (null? positions)
      #t
      (not (check (- k 1)
		  (car positions)
		  k
		  (cdr positions)))))

;; 另一种正确的safe检测函数
(define (safe? k positions)
  (define (iter delta row rest)
    (cond ((null? rest) #t)
          ((= row (car rest)) #f)
          ((= (+ row delta) (car rest)) #f)
          ((= (- row delta) (car rest)) #f)
          (else (iter (+ delta 1) row (cdr rest)))))
  (iter 1 (car positions) (cdr positions)))
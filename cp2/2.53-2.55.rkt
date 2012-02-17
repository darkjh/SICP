;; Ex. 2.53 - 2.55

;; 利用eq？过程定义的函数，返回表x中item第一次出现位置之后的一个子表
(define (memq1 item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Ex. 2.54
;; 递归的判断两个表是否相同
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))



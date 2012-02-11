;; Section 2.2.4
;; A picture language
;; 一个基于Scheme之上的DSL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket GUI Toolkit Test
(require racket/gui/base)

;; 新建一个frame，这里是指一个基本的窗口容器
(define test-frame (new frame%
		   [label "Test"]
		   [width 700]
		   [height 700]))

;; 在上面的frame里添加一个按钮，且能响应点击事件
(define test-button
  (new button% [parent test-frame]
       [label "Test Button"]
       (callback (lambda (button event)
		   (send button set-label "Clicked")))))

;; 建立画图区域
(define test-canvas (new canvas% [parent test-frame]))

;; 取得画图区域里的drawable context
(define test-dc (send test-canvas get-dc))

;; 定义一个画笔
(define red-pen (new pen% [color "red"] [width 1]))

;; 试着画一下
(define (test-draw)
  (send test-dc set-pen red-pen)
  (send test-dc draw-line 0 10 30 10))

;; 建立一个图片的框架
(define (draw-frame)
  (make-frame
    test-dc
    (make-vect 0 0)
    (make-vect 300 0)
    (make-vect 0 300)))

;; (define zee
;;   (let ([z (new dc-path%)])
;;     (send z move-to 0 0)
;;     (send z line-to 100 0)
;;     (send z line-to 0 100)
;;     (send z line-to 100 100)
;;     z))

;; (define test-canvas
;;   (new canvas% [parent test-frame]
;;        [paint-callback
;; 	(lambda (test-canvas dc)
;; 	  (send dc set-brush "blue" 'solid)
;; 	  (send dc set-pen "black" 10 'solid)
;; 	  (send dc draw-path zee)
;; 	  )]))

; Show the frame by calling its show method
(send test-frame show #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 已知一个画家，对它所做的各种变幻

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;; right-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;; Ex. 2.44
;; up-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split painter (- n 1))))
	(below painter (beside upper upper)))))

;; square-of-four
;; 对于4个图像的各种组合的抽象，tl，tr，bl，br分别代表了四个角的图片的变幻方法

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; 借助于square-of-four定义flipped-pairs
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; 也对right-slipt和up-slipt的公共模式进行抽象
;; Ex. 2.45
;; 注意((split first second) painter (- n 1))

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split first second) painter (- n 1))))
	  (first painter (second smaller smaller))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 下层功能函数的实现，以及和GUI模型的结合

;; 框架的坐标映射
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;; Ex. 2.46
;; 向量的数据表示

;; 构造函数及选择函数
(define (make-vect x y)
  (cons x y))
(define (xcor-vect cor)
  (car cor))
(define (ycor-vect cor)
  (cdr cor))

;; 基本操作
(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
		(xcor-vect b))
	     (+ (ycor-vect a)
		(ycor-vect b))))
(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
		(xcor-vect b))
	     (- (ycor-vect a)
		(ycor-vect b))))
(define (scale-vect vect factor)
  (make-vect (* (xcor-vect vect) factor)
	     (* (ycor-vect vect) factor)))

;; Ex. 2.47
;; 框架的实现,加入了dc（drawable context），方便与各类GUI系统结合

;; 构造函数及选择函数
(define (make-frame dc origin edge1 edge2)
  (list dc origin edge1 edge2))
(define (dc-frame frame)
  (car frame))
(define (origin-frame frame)
  (cadr frame))
(define (edge1-frame frame)
  (caddr frame))
(define (edge2-frame frame)
  (cadddr frame))

;; Ex. 2.48
;; 平面线段的实现

;; 构造函数及选择函数
(define (make-segment to-start to-end)
  (list to-start to-end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;; Ex. 2.49

;; 自己定义的draw-line，在给定的dc上画出一条由v1,v2确定的线段
(define (draw-line dc v1 v2)
  (define (draw-segment dc seg)
    (let ((v-start (start-segment seg))
	  (v-end (end-segment seg)))
      (send dc draw-line		; 这个draw-line是dc内部的成员过程
	    (xcor-vect v-start)
	    (ycor-vect v-start)
	    (xcor-vect v-end)
	    (ycor-vect v-end))))
  (draw-segment dc (make-segment v1 v2)))

;; 用draw-line函数把一个线段集合画成图形
(define (segments->painter seg-list)
  (lambda (frame)
    (for-each
     (lambda (seg)
       (draw-line
	(dc-frame frame)
	((frame-coord-map frame) (start-segment seg))
	((frame-coord-map frame) (end-segment seg))))
     seg-list)))


;; 几个图形的画法，使用上面的过程
(define outline
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 1 1) (make-vect 1 0))
          (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (segments->painter
    (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
          (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
          (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
          (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))))

(define wave
  (segments->painter
    (list (make-segment (make-vect 0.60 0.00) (make-vect 0.50 0.30))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00))
          (make-segment (make-vect 0.25 0.00) (make-vect 0.35 0.40))
          (make-segment (make-vect 0.35 0.40) (make-vect 0.30 0.50))
          (make-segment (make-vect 0.30 0.50) (make-vect 0.15 0.35))
          (make-segment (make-vect 0.15 0.35) (make-vect 0.00 0.60))
          (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.55))
          (make-segment (make-vect 0.15 0.55) (make-vect 0.30 0.60))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.40 0.60))
          (make-segment (make-vect 0.40 0.60) (make-vect 0.35 0.80))
          (make-segment (make-vect 0.35 0.80) (make-vect 0.40 1.00))
          (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.80))
          (make-segment (make-vect 0.65 0.80) (make-vect 0.60 0.60))
          (make-segment (make-vect 0.60 0.60) (make-vect 0.75 0.60))
          (make-segment (make-vect 0.75 0.60) (make-vect 1.00 0.40))
          (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.40))
          (make-segment (make-vect 0.60 0.40) (make-vect 0.75 0.00)))))
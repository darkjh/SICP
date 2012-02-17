;; Section 2.2.4
;; A picture language
;; 一个基于Scheme之上的DSL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket GUI Toolkit Test

;; Racket中的图形函数包
(require racket/gui/base)

;; 新建一个frame，这里是指一个基本的窗口容器
(define test-frame (new frame%
		   [label "Test"]
		   [width 700]
		   [height 700]))

;; 在上面的frame里添加一个按钮，点击清空画图区域
(define test-button
  (new button% [parent test-frame]
       [label "Clear Canvas"]
       (callback (lambda (button event)
		   (send test-dc clear)))))

;; 在新建的frame之上建立画图区域canvas
(define test-canvas (new canvas% [parent test-frame]))

;; 取得画图区域里的drawable context，也就是画图函数直接操作的对象
(define test-dc (send test-canvas get-dc))


(define red-pen (new pen% [color "red"] [width 1]))

;; 试着画一下
(define (test-draw)
  (send test-dc set-pen red-pen)
  (send test-dc draw-line 0 10 30 10))

;; 在已经建立的画布canvas里建立一个图片的框架，注意框架中包含画布的dc
(define (draw-frame)
  (make-frame
    test-dc
    (make-vect 0 0)
    (make-vect 700 0)
    (make-vect 0 700)))

;; 调用frame的show方法，显示窗口
(send test-frame show #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 这里是最高的抽象层，我们操作的对象是一个画家painter，根据按愿望思维的原则，我们认为一个画家能够画出各种图形，比如说画家wave或者画家outline
;; 在这个最高一层的抽象层里，已知一个画家，我们定义对它所做的各种变幻

;; 上下颠倒一对画家
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;; 往右侧递归地组合画家，right-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;; Ex. 2.44
;; up-split，于right-split相似，往上递归地组合画家
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((upper (up-split painter (- n 1))))
	(below painter (beside upper upper)))))

;; square-of-four
;; 对于4个图像的各种组合的抽象，tl，tr，bl，br分别代表了四个角（左上，右上，左下，右下）的图片的变幻方法

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; 借助于square-of-four定义的flipped-pairs
;; 我们只需要给上面的square-of-four函数传递4个相应的参数函数，左上左下不变，右上右下角翻转
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; 我们也对right-slipt和up-slipt的公共模式进行抽象
;; Ex. 2.45
;; 注意((split first second) painter (- n 1))

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split first second) painter (- n 1))))
	  (first painter (second smaller smaller))))))


;; 画家变幻的公共模式
;; 以画家和变幻框架以及生成画家的信息为参数，产生新的画家

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame (dc-frame frame)
		     new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

;; 利用公共模式定义不同的变幻
;; 定义翻转变幻
(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
;; 缩小变幻
(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

;; 旋转90度
(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

;; 向内收缩
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

;; 对两个或者更多画家的组合
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;; Ex. 2.50
;; 水平翻转
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
;; 逆时针180度
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
;; 逆时针270度
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; Ex. 2.51
;; below操作
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0)))
	  (paint-down
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
      (lambda (frame)
	(paint-up frame)
	(paint-down frame)))))

;; 另一种实现方法，利用旋转和beside操作
(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 又是一个抽象屏障。在下面的一个抽象层中，我们定义了平面向量，线段，画框的表示方式，同时也定义了标准画框（0到1范围）到任意大小画框的转换函数。这样我们所有定义图形的工作就不需要考虑缩放问题了
;; 画框的函数中需要加入dc，通过这个dc参数，我们把一个画框放进了一个画布canvas之中
;; 下层功能函数的实现，以及和GUI模型的结合

;; 框架的坐标映射，也就是对标准图形表示的一个缩放，以适应任意大小的画框
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
(define (scale-vect s vect)
  (make-vect (* (xcor-vect vect) s)
	     (* (ycor-vect vect) s)))

;; Ex. 2.47
;; 画框的实现,加入了dc（drawable context），方便与各类GUI系统结合

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
  (cadr seg))

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

;; 用draw-line函数把一个线段转变成一个画家，即一个过程，其在参数frame上作画
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
;; 正如上面所说，图形中笔画和点的位置都在单位标准画框中确定
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
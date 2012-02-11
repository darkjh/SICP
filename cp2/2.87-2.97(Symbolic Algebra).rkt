

;;Addition and multiplication
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- Add-Poly"
             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- Mul-Poly"
             (list p1 p2))))
;;Package
(define (install-polynomial-package)
  ;;internal procedures
  ;;representation of body
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v (symbol? v)))
  (define (same-variable? v1 v2)
    (and (variable v1) (variable v2) (eq? v1 v2)))

  ;;representation of terms and term lists

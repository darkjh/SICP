;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

  (print "new-if:")
  (new-if (= 2 3) (print "yes") (print "no"))

  (print "if:")
  (if (= 2 3) (print "yes") (print "no"))
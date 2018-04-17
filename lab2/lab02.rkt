#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab2
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab: Write code
;; which functions are both passed as
;; parameters, and returned as values.
;;;;;;;;;;
(provide I)
(provide D)

(define I
  (lambda (f)
   (let* ((delta 0.00001)
          (two-delta (* 2 delta)))
     (lambda (a b)
       (define calc-riemann
         (lambda (sum pointer)
           (if (> pointer b)
              sum
              (calc-riemann (+ sum (* delta (f pointer))) (+ pointer delta)))))
           (calc-riemann 0.0 a)))))
      
       

(define D
  (lambda (f)
    (let* ((delta 0.00001)
           (two-delta (* 2 delta)))
      (lambda (x)
        (/ (- (f (+ x delta))
              (f (- x delta)))
           two-delta)))))
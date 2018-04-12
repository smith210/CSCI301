#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab1
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab:
;;;;;;;;;;
(provide make-pi)

(define make-pi
    (lambda (limit)
      (define calculate-pi
        (lambda (num dem sum)
          (if (> limit (/ (abs num) dem))
              sum
              (calculate-pi (- num) (+ dem 2.0) (+ sum (/ num dem))))))
      (calculate-pi 4.0 1.0 0.0)))

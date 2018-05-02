#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab4
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab: Get used to
;; tree recursion as an evaluation
;; strategy, and programming it.
;;;;;;;;;;
(provide lookup evaluate)

(define lookup
  (lambda (symbo env)
   ;;base case: if env is empty or not a symbol
    (if (or (not (symbol? symbo)) (empty? env))
        (error "ERROR - following not in env or not a symbol:" symbo)
        (let ((my-symbo (car (car env))))
          ;;check the current symbol is equal to the one we want
          ;;if it is retrieve value, otherwise recursive the call
          (if (eqv? my-symbo symbo)
              (cdr (car env))
              (lookup symbo (cdr env)))))))
         

(define evaluate
  (lambda (express env)
    (cond
      ;;if arguement is a number
      [(number? express) express]
      ;;if arguement is a symbol
      [(symbol? express) (lookup express env)]
      ;;if arguement is a list, process and map
      [(list? express)
         (let ((eval-list (map (lambda (x) (evaluate x env)) express)))
            ;;if the first arguement present is a procedure, apply it to the list
            (cond [(procedure? (car eval-list)) (apply (car eval-list) (cdr eval-list))]))]
      [else (error "ERROR - SOMETHING WENT WRONG IDK WHAT")])))
    
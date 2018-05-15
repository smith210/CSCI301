#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab6
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab: Process some
;; special forms, specifically 'let'.
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

;;get special-form statements to return true
(define special-form?
  (lambda (expression)
    (cond [(not (list? expression)) #f]
          [(member (car expression) '(if cond let lambda letrec)) #t]
          [else #f])))

;;create the new enviornment
(define create-env
  (lambda (new old express)
         (if (empty? (cdr express))
             (env-helper (append (list (cons (car (car express)) (evaluate (cdr (car express)) old))) new) old)
             (create-env (append (list (cons (car (car express)) (evaluate (cdr (car express)) old))) new) old (cdr express)))))

;;creation of enviornment - helper
(define env-helper
  (lambda (new old)
    (append new old)))
         
(define evaluate-special-form
  (lambda (express env)
    (cond [(not (member (car express) '(if cond let))) #f]
          ;;case - if special-form
          [(member (car express) '(if))
             (if (evaluate (car (cdr express)) env)
                      (evaluate (car (cdr (cdr express))) env)
                      (evaluate (cdr (cdr (cdr express))) env))]
          ;;case - cond special-form
          [(member (car express) '(cond))
          ;;have loop that will iterate through cond statement
              (define loop
                 (lambda (in-express env)
                   ;;base case - if gone through all cond statements
                   (if (empty? (cdr in-express))
                         (evaluate (cdr (car in-express)) env)
                         ;;if not empty, evaluate to see if #t or #f
                         (if (evaluate (car (car (cdr in-express))) env)
                              (evaluate (car (cdr (car in-express))) env)
                              ;;else recurse the loop
                              (loop (cdr (cdr in-express)) env)))))
              ;;call the loop
              (loop (cdr express) env)]
          ;;case  - let special-form
          [(member (car express) '(let))
              ;;create new env with the additional values from let statement
              (let ((fresh-env (create-env '() env (car (cdr express)))))
              ;;evaluate expression with the new enviornment
              (evaluate (cdr (cdr express)) fresh-env))]
          [else (error "ERROR - SOMETHING WENT WRONG IDK WHAT")])))

(define evaluate
  (lambda (express env)
    (cond
      ;;if arguement is a number
      [(number? express) express]
      ;;if arguement is a symbol
      [(symbol? express) (lookup express env)]
      ;;Lab5 - include conditions for special-forms
      [(special-form? express) (evaluate-special-form express env)]
      ;;if arguement is a list, process and map
      [(list? express)
         (let ((eval-list (map (lambda (x) (evaluate x env)) express)))
            ;;if the first arguement present is a procedure, apply it to the list
            (cond [(procedure? (car eval-list)) (apply (car eval-list) (cdr eval-list))]
                  [(number? (car eval-list)) (car eval-list)]))]
      [else (error "ERROR - SOMETHING WENT WRONG IDK WHAT")])))
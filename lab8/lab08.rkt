#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab8
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab: Process some
;; special forms, specifically 'letrec'.
;;;;;;;;;;

(provide lookup evaluate)
;;create closure - use for lambda and letrec cases
(struct closure (vars body (envio #:mutable)))

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map cons
                '(x y z + - * cons car cdr nil = equal? < else  add list)
                (list 2 4 6 + - * cons car cdr '() = equal? < #t    add list)))

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
    (if (empty? express)
        (append new old)
        (create-env (append (list (cons (car (car express)) (evaluate (car (cdr (car express))) old))) new) old (cdr express)))))

;;map and evaluate the closure
(define apply-closure
  (lambda (cls lst)
    (let ((temp-list (map cons (closure-vars cls) lst)))
      (evaluate (closure-body cls) (append temp-list (closure-envio cls))))))

;;letrec remodel - reset the enviornment of the closures w/ new enviornment
(define letrec-remodel
  (lambda (letrec-env newrec-env)
    (if (empty? letrec-env)
        newrec-env
        ;;set the enviornment of the closure to the new enviornment
        (if (closure? (cdr (car letrec-env)))
            (begin
              (set-closure-envio! (cdr (car letrec-env)) newrec-env)
              (letrec-remodel (cdr letrec-env) newrec-env))
            (letrec-remodel (cdr letrec-env) newrec-env)))))

;;letrec assist - create the new enviornment of letrec symbols
(define letrec-assist
  (lambda (letrec-states env letrec-env)
    ;;create the new enviornment with the symbols from letrec
    (if (empty? letrec-states)
        (letrec-remodel letrec-env (append letrec-env env))
        (letrec-assist (cdr letrec-states) env (append letrec-env (list (cons (car (car letrec-states)) (evaluate (car (cdr (car letrec-states))) env))))))))

;;letrec helper - evaluate the function
(define letrec-helper
  (lambda (eval env)
    ;;pull symbols and closures out of eval, call letrec-assist
    (let ((new-env (letrec-assist (car eval) env '())))
      ;;evaluate the call with the new enviornment
      (evaluate (car (cdr eval)) new-env))))

;;evaluate when form is special
(define evaluate-special-form
  (lambda (express env)
    (cond [(not (member (car express) '(if cond let lambda letrec))) #f]
          ;;case - if special-form
          [(member (car express) '(if))
           (if (evaluate (car (cdr express)) env)
               (evaluate (car (cdr (cdr express))) env)
               (evaluate (car (cdr (cdr (cdr express)))) env))]
          ;;case - cond special-form
          [(member (car express) '(cond))
           (define loop
             (lambda (in-express env)
               ;;base case - if gone through all cond statements
               (if (empty? (cdr in-express))
                   (evaluate (cdr (car in-express)) env)
                   ;;if not empty, evaluate if #t or #f
                   (if (evaluate (car (car (cdr in-express))) env)
                       (evaluate (car (cdr (car in-express))) env)
                       (loop (cdr (cdr in-express)) env)))))
           (loop (cdr express) env)]
          ;;case  - let special-form
          [(member (car express) '(let))
           ;;create new env
           (let ((fresh-env (create-env '() env (car (cdr express)))))
             ;;evaluate with new enviornment
             (evaluate (car (cdr (cdr express))) fresh-env))]
          ;;case - lambda special-form
          [(member (car express) '(lambda))
           ;;create closure
           (closure (car (cdr express)) (car (cdr (cdr express))) env)]
          ;;case - letrec special-form
          [(member (car express) '(letrec))
           ;;pull the function from the expression
           (letrec-helper (cdr express) env)]
          [else (error "ERROR - evaluate-special-form")])))

(define evaluate
  (lambda (express env)
    
    (cond
      ;;if arguement is a number
      [(number? express) express]
      ;;if arguement is a symbol
      [(symbol? express) (lookup express env)]
      ;;if arguement is a special-form
      [(special-form? express) (evaluate-special-form express env)]
      ;;if arguement is a list, process and map
      [(list? express)
       (let ((eval-list (map (lambda (x) (evaluate x env)) express)))
         (cond [(procedure? (car eval-list)) (apply (car eval-list) (cdr eval-list))]
               [(number? (car eval-list)) (car eval-list)]
               [(closure? (car eval-list)) (apply-closure (car eval-list) (cdr eval-list))]))]
      [else (error "ERROR - evaluate")])))
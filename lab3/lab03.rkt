#lang racket
;;;;;;;;;;
;; CSCI 301 - Spring 2018
;;
;; Assignment: Lab3
;;
;; Name: Martin Smith
;;       W01322980
;;
;; Purpose of the lab: Write code
;; using lists. Use car, cdr, and
;; recursion to traverse.
;;;;;;;;;;
(provide sublists subsets distribute length-ordered? element-ordered?)

(define (distribute append-lst og-lst)
  ;;if og-lst is the last element (or only element)
  (if (empty? (cdr og-lst))
      ;;create a list with append-lst as previous element
      (cons (cons append-lst (car og-lst)) '())
      ;;append to the list using recursion
      (cons (cons append-lst (car og-lst))
            (distribute append-lst (cdr og-lst)))))

(define element-ordered?
  (lambda (lst1 lst2)
  ;;if lst1 is empty, means that all checks went through satisfactory, return #t
  (if (empty? lst1)
      #t
      ;;Make sure that lst2 is not empty before doing second check.
      ;;Also check so that the first element in lst1 is smaller than first element in lst2,
      ;;If it isn't so, return #f
      (if (or (empty? lst2) (> (car lst1) (car lst2)))
           #f
           (element-ordered? (cdr lst1)(cdr lst2))))))

(define (length-ordered? lst1 lst2)
  ;;case: if less elements in lst1 than lst2, return #t
  (if (and (empty? lst1) (not (empty? lst2)))
          #t
       ;;case: if less elements in lst2 than lst1, return #f
       (if (empty? lst2)
          #f
          (length-ordered? (cdr lst1) (cdr lst2)))))

(define (sublists lst)
  ;;base case: if nothing is in lst
  (if (empty? lst)
      (cons '() '())
      ;;create list my-lst to create the powerset (and save subsets that we know to exist)
      (let ((my-lst (sublists (cdr lst))))
        ;;get all the sets by distruting the first element (car of lst) across my-lst
        (append (distribute (car lst) my-lst) my-lst))))

(define (subsets lst)
  ;;use sort function (provided by racket), start with length-ordered?, then element-ordered?
  (sort (sort (sublists lst) length-ordered?) element-ordered?))
      
      
#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (map string->list (read-file-lines "input")))

(define (check-line l [prev '()])
  (if (= (length l) 0) #f
    (let ([oc
      (case (car l) [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [(#\<) #\>] [else #f])])
      (if oc (check-line (cdr l) (cons oc prev))
        (if (eq? (car l) (car prev)) (check-line (cdr l) (cdr prev))
          (car l))))))

(apply + (let ([score (hash #\) 3 #\] 57 #\} 1197 #\> 25137)])
  (map (curry hash-ref score) (filter identity (map check-line input)))))

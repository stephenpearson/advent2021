#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (map string->list (read-file-lines "input")))

(define (check-line l [prev '()])
  (if (= (length l) 0) prev
    (let ([oc (case (car l)
      [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [(#\<) #\>] [else #f])])
      (if oc (check-line (cdr l) (cons oc prev))
        (if (eq? (car l) (car prev)) (check-line (cdr l) (cdr prev))
          #f)))))

(let ([scores (sort (let ([score (hash #\) 1 #\] 2 #\} 3 #\> 4)])
  (map (λ (l) (foldl (λ (a b) (+ (* b 5) (hash-ref score a))) 0 l))
    (filter identity (map check-line input)))) <)])
  (list-ref scores (quotient (length scores) 2)))

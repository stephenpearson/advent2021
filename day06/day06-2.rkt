#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input
 (map string->number (car (map csv->list (read-file-lines "input")))))

(define (list-add-nth l v n)
  (list-set l n (+ (list-ref l n) v)))

(define (parse-input input [result (make-list 10 0)])
  (if (= (length input) 0) result
    (parse-input (cdr input) (list-add-nth result 1 (car input)))))

(define (iterate l)
  (append (list-add-nth (list-add-nth (cdr l) (car l) 6) (car l) 8) '(0)))

(define (run input n)
  (if (> n 0) (run (iterate input) (- n 1)) input))

(apply + (run (parse-input input) 256))

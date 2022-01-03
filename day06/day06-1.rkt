#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input
  (map string->number (car (map csv->list (read-file-lines "input")))))

(define (iterate l [new 0])
  (if (> (length l) 0)
    (if (> (car l) 0)
      (cons (- (car l) 1) (iterate (cdr l) new))
      (cons 6 (iterate (cdr l) (add1 new))))
    (for/list ([i (in-range new)]) 8)))

(define (run input n)
  (if (> n 0)
    (run (iterate input) (- n 1))
    (length input)))

(run input 80)

#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define grid (list->vector (map (lambda (l) (list->vector
  (map (compose string->number string) (string->list l))))
    (read-file-lines "input"))))

(define my (vector-length grid))
(define mx (vector-length (vector-ref grid 0)))

(define (get grid x y)
  (if (or (< y 0) (>= y my) (< x 0) (>= x mx)) 9
    (vector-ref (vector-ref grid y) x)))

(apply + (map add1 (filter identity
  (for*/list ([x (in-range mx)] [y (in-range my)])
    (let ([c (get grid x y)])
      (if (for/and ([d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
        (< c (get grid (+ x (car d)) (+ y (cdr d))))) c #f))))))

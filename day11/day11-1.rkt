#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (map (λ (l)
  (map (compose string->number string) (string->list l)))
    (read-file-lines "input")))
(define mx (length (car input)))
(define my (length input))

(define (apply-grid grid fn)
  (make-hash (for*/list ([x (in-range mx)] [y (in-range my)])
    (cons (cons x y) (fn grid x y (hash-ref grid (cons x y) 0))))))

(define grid (apply-grid #hash()
  (λ (g x y v) (list-ref (list-ref input y) x))))

(define (count-flash grid x y)
  (apply + (for*/list ([a '(-1 0 1)] [b '(-1 0 1)])
    (if (and (= a 0) (= b 0)) 0
      (if (= (hash-ref grid (cons (+ a x) (+ b y)) 0) 10) 1 0)))))

(define (flash-grid grid)
  (if (member 10 (hash-values grid))
    (flash-grid (apply-grid grid (λ (g x y v)
      (if (< v 10) (min (+ v (count-flash g x y)) 10) 11)))) grid))

(define (iterate grid)
  (apply-grid (flash-grid (apply-grid grid (λ (g x y v) (add1 v))))
    (λ (g x y v) (if (> v 9) 0 v))))

(define (count-flashes grid n)
  (if (= n 0) 0
    (let ([g (iterate grid)])
      (+ (length (filter zero? (hash-values g))) (count-flashes g (sub1 n))))))

(count-flashes grid 100)

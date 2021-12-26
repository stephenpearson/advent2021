#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define data
  (map (λ (x) (map (λ (y)
    (map string->number (string-split y ","))) (string-split x " -> ")))
      (read-file-lines "input")))

(define (points line)
  (let ([x1 (caar line)] [x2 (caadr line)] [y1 (cadar line)] [y2 (cadadr line)]
        [line-range (λ (i j) (in-inclusive-range i j (if (< j i) -1 1)))])
    (case (list (= x1 x2) (= y1 y2))
      [((#t #f)) (for/list ([y (line-range y1 y2)]) (list x1 y))]
      [((#f #t)) (for/list ([x (line-range x1 x2)]) (list x y1))]
      [else (for/list ([x (line-range x1 x2)] [y (line-range y1 y2)]) (list x y))])))

(let ([grid (make-hash)])
  (for ([l (map points data)])
    (for ([p l])
      (hash-set! grid p (add1 (hash-ref grid p 0)))))
  (apply + (for/list ([(k v) grid]) (if (> v 1) 1 0))))

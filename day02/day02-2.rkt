#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (submarine horz depth aim l)
  (if (null? l) (list horz depth)
    (let ([lr (cdr l)] [d (caar l)] [l (cadar l)])
      (cond
        [(equal? d 'forward) (submarine (+ horz l) (+ depth (* aim l)) aim lr)]
        [(equal? d 'down) (submarine horz depth (+ aim l) lr)]
        [(equal? d 'up) (submarine horz depth (- aim l) lr)]))))

(apply *
  (submarine 0 0 0
    (read-file-lines-map-with "input" (list string->symbol string->number))))

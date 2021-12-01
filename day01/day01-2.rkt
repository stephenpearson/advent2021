#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (search l)
  (if (> (length l) 3)
    (+ (if (> (apply + (take (cdr l) 3)) (apply + (take l 3))) 1 0)
      (search (cdr l)))
    0))

(let ([data (read-file-of-numbers "./input")])
  (search data))

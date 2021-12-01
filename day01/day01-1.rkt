#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (search f l)
  (if (null? l) 0
    (+ (if (> (car l) (car f)) 1 0) (search l (cdr l)))))

(let ([data (read-file-of-numbers "./input")])
  (search data (cdr data)))

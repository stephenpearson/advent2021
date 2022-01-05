#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (parse-line l)
  (map string-length (string-split (last (string-split l " | ")))))

(define input
  (map parse-line (read-file-lines "input")))

(define counts
  (let ([result (make-hash)])
    (for ([v (flatten input)])
      (hash-set! result v (add1 (hash-ref result v 0)))) result))

(let ([fn (curry hash-ref counts)])
  (apply + (map fn '(2 3 4 7))))

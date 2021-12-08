#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define (bcount lst pos v1 v2)
  (if (>= (/ (length lst) 2)
    (count (curry equal? #\0) (map (λ (l) (list-ref l pos)) lst))) v1 v2))

(define (findval vals v1 v2 [pos 0])
  (if (= (length vals) 1) vals
    (findval (filter (λ (l) (equal? (list-ref l pos) (bcount vals pos v1 v2)))
      vals) v1 v2 (+ pos 1))))

(let*
  ([input (map string->list (read-file-lines "input"))])
    (apply * (map (λ (v) (string->number (list->string
      (car (findval input (car v) (cdr v)))) 2)) '((#\0 . #\1) (#\1 . #\0)))))

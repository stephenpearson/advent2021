#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (foldl (λ (v a)
  (cons (cons (car v) (cadr v)) (cons (cons (cadr v) (car v)) a)))
    '() (map (λ (l) (map string->symbol (string-split l "-")))
      (read-file-lines "input"))))

(define vertex (make-hash (foldl (λ (v a)
  (cons (cons v (map cdr (filter (λ (x) (equal? (car x) v)) input))) a))
    '() (remove-duplicates (map car input)))))

(define (small? s)
  (let ([c (symbol->string s)])
    (equal? (string-downcase c) c)))

(define (paths vertex node [prev '()])
  (if (or (equal? node 'end) (and (small? node) (member node prev))) (cons node prev)
    (apply append (for/list ([n (hash-ref vertex node)])
      (paths vertex n (cons node prev))))))

(length (filter (curry equal? 'end) (paths vertex 'start)))

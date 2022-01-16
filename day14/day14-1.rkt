#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (read-file-lines "input"))

(define template (string->list (car input)))

(define rules (make-hash
  (map (λ (l) (map string->list (string-split l " -> "))) (cddr input))))

(define (iterate template rules)
  (if (= (length template) 1) template
    (let* ([l (car template)] [r (cadr template)]
           [c (hash-ref rules (list l r) #f)])
      (append (if c (list l (caar c)) (list l)) (iterate (cdr template) rules)))))

(define (iterate-n template rules n)
  (if (= n 0) template
    (iterate-n (iterate template rules) rules (- n 1))))

(define (count l)
  (map (λ (v) (length (filter (curry equal? v) l))) l))

(define totals
  (let ([c (iterate-n template rules 10)])
    (for/list ([k (hash-keys (make-hash (map (λ (v) (cons v #t)) c)))])
      (length (filter (curry equal? k) c)))))

(let ([t (sort totals <)])
  (- (last t) (car t)))

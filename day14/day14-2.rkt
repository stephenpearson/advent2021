#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (read-file-lines "input"))

(define rules
  (make-hash (map (λ (l) (cons (cons (caar l) (cadar l)) (caadr l)))
    (map (λ (l) (map string->list (string-split l " -> "))) (cddr input)))))

(define (pairs [t (string->list (car input))])
  (if (= (length t) 1) '()
    (cons (cons (car t) (cadr t)) (pairs (cdr t)))))

(define (uniq l) (hash-keys (make-hash (for/list ([v l]) (cons v #t)))))

(define (count-list l)
  (for/list ([k (uniq l)])
    (cons k (length (filter (curry equal? k) l)))))

(define (simplify l)
  (for/list ([k (uniq (map car l))])
    (cons k (apply + (map cdr (filter (λ (v) (equal? (car v) k)) l))))))

(define (iterate l rules)
  (simplify (apply append (for/list ([i l])
    (let ([r (hash-ref rules (car i) #f)])
      (if r (list (cons (cons (caar i) r) (cdr i)) (cons (cons r (cdar i)) (cdr i)))
        i))))))

(define (iterate-n l rules n)
  (if (= n 0) l
    (iterate-n (iterate l rules) rules (- n 1))))

(define (summarize l t)
  (let ([k (uniq (map caar l))]
        [s (cons (cons t 1) (map (λ (v) (cons (caar v) (cdr v))) l))])
    (for/list ([i k])
      (cons i (apply + (map cdr (filter (λ (x) (equal? (car x) i)) s)))))))

(let ([z (sort (map cdr (summarize
  (iterate-n (count-list (pairs)) rules 40) (cdr (last (pairs))))) <)])
  (- (last z) (car z)))

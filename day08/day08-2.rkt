#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input
  (map (lambda (l) (map (lambda (x) (map (lambda (y)
    (map (compose string->symbol string) y))
      (map string->list (string-split x)))) (string-split l " | ")))
        (read-file-lines "input")))

(define lookup (hash '(a b c e f g)   0   '(c f)         1
                     '(a c d e g)     2   '(a c d f g)   3
                     '(b c d f)       4   '(a b d f g)   5
                     '(a b d e f g)   6   '(a c f)       7
                     '(a b c d e f g) 8   '(a b c d f g) 9))

(define (remap wires mapping)
  (let* ([wset '(a b c d e f g)]
         [wiremap (for/hash ([k wset] [v mapping]) (values k (list-ref wset v)))])
    (sort (map (lambda (x) (hash-ref wiremap x)) wires) symbol<?)))

(define (findmap line [maps (permutations (range 7))])
  (if (= (length maps) 0) #f
    (if (for/and
      ([d (map (lambda (w) (remap w (car maps))) line)])
        (hash-ref lookup d #f)) (car maps) (findmap line (cdr maps)))))

(define (list->number l [v 0])
  (if (= (length l) 0) v
    (list->number (cdr l) (+ (* v 10)(car l)))))

(define (calc input)
  (if (= (length input) 0) 0
    (+ (list->number (map (lambda (v)
      (hash-ref lookup (remap v (findmap (caar input))))) (cadar input)))
      (calc (cdr input)))))

(calc input)

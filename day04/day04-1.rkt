#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (read-file-lines "input"))

(define numbers (map string->number (string-split (car input) ",")))

(define (parse-input input [result '()] [acc '()])
  (if (= (length input) 0)
    (cons acc result)
    (if (equal? "" (car input))
      (parse-input (cdr input) (cons acc result) '())
      (parse-input (cdr input) result (cons (map string->number (string-split (car input))) acc)))))

(define (score board numbers)
  (* (last numbers)
    (apply + (filter (lambda (v) (not (index-of numbers v))) (flatten board)))))

(define (win? board numbers)
  (for/or ([row (append board (apply map list board))])
    (for/and ([i row]) (index-of numbers i))))

(define (find-win boards numbers)
  (if (= (length boards) 0) #f
    (if (win? (car boards) numbers) (score (car boards) numbers)
      (find-win (cdr boards) numbers))))

(for/or ([l (in-range 1 (add1 (length numbers)))])
  (find-win (parse-input (cddr input)) (take numbers l)))

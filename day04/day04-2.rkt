#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input (read-file-lines "input"))

(define (parse-input input [result '()] [acc '()])
  (if (= (length input) 0)
    (cons acc result)
    (if (equal? "" (car input))
      (parse-input (cdr input) (cons acc result) '())
      (parse-input (cdr input) result (cons (map string->number (string-split (car input))) acc)))))

(define numbers (map string->number (string-split (car input) ",")))
(define boards (parse-input (cddr input)))

(define (score numbers board)
  (* (last numbers)
    (apply + (filter (lambda (v) (not (index-of numbers v))) (flatten board)))))

(define (win? board numbers)
  (for/or ([row (append board (apply map list board))])
    (for/and ([i row]) (index-of numbers i))))

(define (find-win boards numbers)
  (if (= (length boards) 0) '()
    (cons
      (if (win? (car boards) numbers) (car boards) #f)
        (find-win (cdr boards) numbers))))

(define result1 (for/list ([l (in-range 1 (add1 (length numbers)))])
  (find-win boards (take numbers l))))

(define result2 (map (lambda (l) (filter identity l)) result1))

(define (find-last data prev maxlen depth)
  (if (= (length data) 0) #f
    (if (= (length (car data)) maxlen)
      (printf "~a\n" (score (take numbers depth) (car (filter (lambda (x) (not (index-of prev x))) (car data)))))
      (find-last (cdr data) (car data) maxlen (add1 depth)))))

(find-last result2 '() (length (last result1)) 1)

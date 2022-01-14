#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define points
  (make-hash (map (lambda (l) (cons l #t))
    (map (lambda (l) (map string->number l))
      (filter (lambda (l) (= (length l) 2))
        (map (lambda (l) (string-split l ",")) (read-file-lines "input")))))))

(define foldings
  (map (lambda (l) (let ([v (string->number (cadr (string-split l "=")))])
    (if (equal? (string-ref l 11) #\x) (cons v #f) (cons #f v))))
      (filter (lambda (l) (and (> (string-length l) 0) (equal? (string-ref l 0) #\f)))
        (read-file-lines "input"))))

(define (fold points f)
  (make-hash (map (lambda (l) (cons l #t))
    (for/list ([p (hash-keys points)])
      (if (car f)
        (if (> (car p) (car f)) (cons (- (car f) (- (car p) (car f))) (cdr p)) p)
        (if (> (cadr p) (cdr f)) (list (car p) (- (cdr f) (- (cadr p) (cdr f)))) p))))))

(define (apply-folds points f)
  (if (= (length f) 0) points
    (apply-folds (fold points (car f)) (cdr f))))

(define (plot points)
  (for ([y (in-range 6)])
    (for ([x (in-range 50)])
      (printf "~a" (if (hash-ref points (list x y) #f) "#" ".")))
    (printf "\n")))

(plot (apply-folds points foldings))

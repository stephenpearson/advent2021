#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define grid (list->vector (map (lambda (l) (list->vector
  (map (compose string->number string) (string->list l))))
    (read-file-lines "input"))))

(define my (vector-length grid))
(define mx (vector-length (vector-ref grid 0)))

(define (get grid x y)
  (if (or (< y 0) (>= y my) (< x 0) (>= x mx)) 9
    (vector-ref (vector-ref grid y) x)))

(define (basin x y)
  (let* ([c (get grid x y)]
         [adj (filter identity
            (for/list ([d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
              (let ([nx (+ x (car d))] [ny (+ y (cdr d))])
                (if (> c (get grid nx ny)) (cons nx ny) #f))))])
    (if (= c 9) #f
      (if (= (length adj) 0) (cons x y)
        (basin (caar adj) (cdar adj))))))

(let ([basins (make-hash)])
  (for ([b (filter identity
         (for*/list ([y (in-range my)] [x (in-range mx)])
         (basin x y)))])
    (hash-set! basins b (add1 (hash-ref basins b 0))))
  (apply * (take (sort (hash-values basins) >) 3)))

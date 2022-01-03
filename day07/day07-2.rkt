#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(define input
  (map string->number (car (map csv->list (read-file-lines "input")))))

(define (val a b)
  (let ([n (abs (- a b))])
    (/ (* n (add1 n)) 2)))

(define (cost l v)
  (if (= (length l) 0) 0 (+ (val (car l) v) (cost (cdr l) v))))

(define seq
  (let ([sinput (sort input <)])
    (in-range (first sinput) (last sinput))))

(define (min-list l)
  (if (= (length l) 1) (car l)
    (min (car l) (min-list (cdr l)))))

(min-list (for/list ([i seq]) (cost input i)))

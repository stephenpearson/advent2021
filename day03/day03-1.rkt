#!/usr/bin/env racket

#lang racket

(require "../common/read_files.rkt")

(let*
  ([input (map string->list (read-file-lines "input"))]
   [epsilon (map (λ (l)
     (if (> (/ (length input) 2)
       (count (curry equal? #\1) l)) #\1 #\0)) (apply map list input))]
   [gamma (map (λ (n) (if (equal? n #\1) #\0 #\1)) epsilon)])
  (apply * (map (λ (l) (string->number (list->string l) 2))
    `(,epsilon ,gamma))))

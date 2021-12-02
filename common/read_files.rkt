#lang racket

(provide read-file-lines)
(provide read-file-of-numbers)
(provide read-file-lines-map-with)

(define (read-file-lines filename)
  (let ([in (open-input-file filename)])
    (string-split (port->string in #:close? #t) "\n")))

(define (read-file-of-numbers filename)
  (map string->number (read-file-lines filename)))

(define (parselines lines fns)
  (map (lambda (line)
    (for/list ([f fns] [l (string-split line)])
      (f l))) lines))

(define (read-file-lines-map-with filename fns)
  (parselines (read-file-lines filename) fns))

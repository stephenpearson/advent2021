#lang racket

(provide read-file-lines)
(provide read-file-of-numbers)

(define (read-file-lines filename)
  (let ([in (open-input-file filename)])
    (string-split (port->string in #:close? #t))))

(define (read-file-of-numbers filename)
  (map string->number (read-file-lines filename)))

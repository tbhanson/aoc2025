#lang racket

(require rackunit "day2.rkt")

(check-true
 (stream-empty?
  (read-ID-ranges (open-input-string ""))))

(check-equal?
 (stream->list
  (read-ID-ranges (open-input-string "1-2")))
 (list (cons 1 2)))

(check-equal?
 (stream->list
  (read-ID-ranges (open-input-string "1-2,11-345")))
 (list (cons 1 2) (cons 11 345)))


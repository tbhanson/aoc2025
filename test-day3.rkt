#lang racket

(require rackunit "day3.rkt")

;; (check-true
;;  (stream-empty?
;;   (read-ID-ranges (open-input-string ""))))
;; 
(check-equal?
 (max-joltage "123")
 23)

(check-equal?
 (max-joltage "987654321111111")
 98)

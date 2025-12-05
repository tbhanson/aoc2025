#lang racket

(require rackunit "day3.rkt")

; max-joltage
(check-equal?
 (max-joltage "123")
 23)

(check-equal?
 (max-joltage "987654321111111")
 98)

; total-joltage
; test their sample input
(let ([stream-of-joltage-strings
       (read-joltage-strings
        (open-input-string "987654321111111 811111111111119 234234234234278 818181911112111"))])
  (check-equal?
   (total-joltage stream-of-joltage-strings)
   357))

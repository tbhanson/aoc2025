#lang racket

(require rackunit "day2.rkt")

; read-ID-ranges
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

; invalid-ID
(check-false
 (invalid-ID 1))
 
(check-false
 (invalid-ID 12))
 
(check-true
 (invalid-ID 11))

; do we need to care about this? ("not an ID at all")
(check-false
 (invalid-ID 0101))

(check-false
 (invalid-ID 123))
 
(check-true
 (invalid-ID 1212))

; invalid-IDs-in-range
(check-equal?
 (stream->list
  (invalid-IDs-in-range 11 22))
 (list 11 22))

(check-equal?
 (stream->list
  (invalid-IDs-in-range 95 115))
 (list 99))

; sum-invalid-IDs-in-ID-range-stream
(check-equal?
 (sum-invalid-IDs-in-ID-range-stream
  (read-ID-ranges (open-input-string "11-22,95-115")))
 (+ 11 22 99))

; check their small test input
(check-equal?
 (sum-invalid-IDs-in-ID-range-stream
  (read-ID-ranges
   (open-input-string 
    (string-append
     "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
     "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
     "824824821-824824827,2121212118-2121212124"))))
 1227775554)

; check against their "real" input
;; (check-equal?
;;  (sum-invalid-IDs-in-ID-range-stream
;;   (read-ID-ranges
;;    (open-input-file "test-data/input-day2-1.txt")))
;;  17)


; invalid-ID-2
(check-false
 (invalid-ID-2 12))
 
(check-true
 (invalid-ID-2 11))

(check-true
 (invalid-ID-2 111))

; check their small test input
(check-equal?
 (sum-invalid-IDs-2-in-ID-range-stream
  (read-ID-ranges
   (open-input-string 
    (string-append
     "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
     "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
     "824824821-824824827,2121212118-2121212124"))))
 4174379265)


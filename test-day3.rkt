#lang racket

(require rackunit "day3.rkt")

; max-joltage
(check-equal?
 (max-joltage "123")
 23)

(check-equal?
 (max-joltage "987654321111111")
 98)

(check-equal?
 (max-joltage
  "3433224132322232224331231232231222332222224131333331131333222223422234232214231522223253221331123231")
  55)

(check-equal?
 (max-joltage
  "1222232222222313223322332222223323322221225113712423532123211131122223232213122312222242122311232121")
  75)

(check-equal?
 (max-joltage
  "4333222423523533313524542534333525333493263235233222333433156321242333473335522353313753221232423335")
  97)




; total-joltage
; test their sample input
(let ([stream-of-joltage-strings
       (read-joltage-strings
        (open-input-string (format "987654321111111~n811111111111119 234234234234278 818181911112111")))])
  (check-equal?
   (total-joltage stream-of-joltage-strings)
   357))

(let ([stream-of-joltage-strings
       (read-joltage-strings
        (open-input-string
         (string-append
          "3433224132322232224331231232231222332222224131333331131333222223422234232214231522223253221331123231 "
          "1222232222222313223322332222223323322221225113712423532123211131122223232213122312222242122311232121 "
          "4333222423523533313524542534333525333493263235233222333433156321242333473335522353313753221232423335")))])
  (check-equal?
   (total-joltage stream-of-joltage-strings)
   227))

; test day 3 part 1
;; (let ([stream-of-joltage-strings
;;        (read-joltage-strings
;;         (open-input-file "test-data/input-day3-1.txt"))])
;;   (check-equal?
;;    (total-joltage stream-of-joltage-strings)
;;    ---)) ; my first computed answer was wrong (I had naively thought the strings would be at most 99 digits long!!)

; test their updated small test cases
; max-joltage-2
(check-equal?
 (max-joltage-2 "987654321111111")
 987654321111)

(check-equal?
 (max-joltage-2 "811111111111119")
 811111111119)

(check-equal?
 (max-joltage-2 "234234234234278")
 434234234278)

(check-equal?
 (max-joltage-2 "818181911112111")
 888911112111)

; test day 3 part 2
;; (let ([stream-of-joltage-strings
;;        (read-joltage-strings
;;         (open-input-file "test-data/input-day3-1.txt"))])
;;   (check-equal?
;;    (total-joltage-2 stream-of-joltage-strings)
;;    ...))





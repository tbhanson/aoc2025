#lang racket

(require rackunit "day1.rkt")

(check-true
 (stream-empty?
  (read-lock-rotations (open-input-string ""))))

(check-equal?
 (stream->list
  (read-lock-rotations (open-input-string "R1")))
 (list (lock-rotation #\R 1)))

(check-equal?
 (stream->list
  (read-lock-rotations (open-input-string "R17 L32")))
 (list (lock-rotation #\R 17)
       (lock-rotation #\L 32)
       ))

; read-lock-rotations
(check-true
 (stream?
  (read-lock-rotations (open-input-file "test-data/input-day1-1.txt"))))

(check-equal?
 (stream-length
  (read-lock-rotations (open-input-file "test-data/input-day1-1.txt")))
 4531)

; next-position
(check-equal?
 (next-position 0 (lock-rotation #\R 5))
 5)

(check-equal?
 (next-position 0 (lock-rotation #\L 5))
 95)

(check-equal?
 (next-position 50 (lock-rotation #\R 233))
 83)

; get-password-from-lock-rotations -- test on their small sample input 

(check-equal?
 (get-password-from-lock-rotations-part1
  (read-lock-rotations (open-input-string "L68 L30 R48 L5 R60 L55 L1 L99 R14 L82 ")))
 3)

;; (check-equal?
;;  (get-password-from-lock-rotations-part1
;;   (read-lock-rotations (open-input-file "test-data/input-day1-1.txt")))
;;  what me be a spoiler? )

; (next-position-and-zero-count current-position next-lock-rotation)

(check-true
 (let-values ([(new-pos zero-hits)
               (next-position-and-zero-count 0 (lock-rotation #\R 5))])
   (and (= new-pos 5)
        (= zero-hits 0))))

(check-true
 (let-values ([(new-pos zero-hits)
               (next-position-and-zero-count 0 (lock-rotation #\L 5))])
   (and (= new-pos 95)
        (= zero-hits 0))))

(check-true
 (let-values ([(new-pos zero-hits)
               (next-position-and-zero-count 1 (lock-rotation #\L 3))])
   (and (= new-pos 98)
        (= zero-hits 1))))

; get-password-from-lock-rotations-part2
(check-equal?
 (get-password-from-lock-rotations-part2
  (read-lock-rotations (open-input-string "L68 L30 R48 L5 R60 L55 L1 L99 R14 L82 ")))
 6)

;; (check-equal?
;;  (get-password-from-lock-rotations-part2
;;   (read-lock-rotations (open-input-file "test-data/input-day1-1.txt")))
;;  what me be a spoiler? )


#lang racket

(require rackunit "day4.rkt")

; read in grid
(let ([in-port
       (open-input-string
        (format
         (string-append
          "..@@~n"
          "@@@.~n"
          "@@@@~n")))])
  (let ([forklift-grid
         (read-forklift-grid in-port)])       
    (check-equal?
     forklift-grid
     (list->vector
      (list
       (list->vector
        (string->list  "..@@"))
       (list->vector
        (string->list  "@@@."))
       (list->vector
        (string->list  "@@@@")))))

    (check-equal?
     (adjacent-roll-count forklift-grid 0 1)
     4)

    (check-equal?
     (accessible-roll-count forklift-grid)
     5)
    ))


; their small example
(let ([in-port
       (open-input-string
        (format
         (string-append
          "..@@.@@@@.~n"
          "@@@.@.@.@@~n"
          "@@@@@.@.@@~n"
          "@.@@@@..@.~n"
          "@@.@@@@.@@~n"
          ".@@@@@@@.@~n"
          ".@.@.@.@@@~n"
          "@.@@@.@@@@~n"
          ".@@@@@@@@.~n"
          "@.@.@@@.@.~n")))])
  (let ([forklift-grid
         (read-forklift-grid in-port)])       
    (check-equal?
     (accessible-roll-count forklift-grid)
     13)
    ))

; test day 4 part 1
;; (let ([forklift-grid
;;        (read-forklift-grid
;;         (open-input-file "test-data/input-day4-1.txt"))])       
;;   (check-equal?
;;    (accessible-roll-count forklift-grid)
;;    ---)
;;   )

; part 2
(let ([in-port
       (open-input-string
        (format
         (string-append
          "..@@~n"
          "@@@.~n"
          "@@@@~n")))])
  (let ([forklift-grid
         (read-forklift-grid in-port)])       
    (check-false
     (accessible-roll-at-xy? forklift-grid 0 1))

    (check-true
     (accessible-roll-at-xy? forklift-grid 0 2))

    (check-false
     (accessible-roll-at-xy? forklift-grid 1 1))

    ))
    


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
   
  
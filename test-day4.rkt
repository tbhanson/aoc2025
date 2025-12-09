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
  (check-equal?
   (read-forklift-grid in-port)
   (list->vector
    (list
     (list->vector
      (string->list  "..@@"))
     (list->vector
      (string->list  "@@@."))
     (list->vector
      (string->list  "@@@@"))))))
     
    
   
   

;; ..@@.@@@@.
;; @@@.@.@.@@
;; @@@@@.@.@@
;; @.@@@@..@.
;; @@.@@@@.@@
;; .@@@@@@@.@
;; .@.@.@.@@@
;; @.@@@.@@@@
;; .@@@@@@@@.
;; @.@.@@@.@.
  
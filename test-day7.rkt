#lang racket

(require rackunit "day7.rkt")

; part 1
(let ([in-port
       (open-input-string
        (format
         (string-append
          ".......S.......~n"
          "...............~n"
          ".......^.......~n"
          "...............~n"
          "......^.^......~n"
          "...............~n"
          ".....^.^.^.....~n"
          "...............~n"
          "....^.^...^....~n"
          "...............~n"
          "...^.^...^.^...~n"
          "...............~n"
          "..^...^.....^..~n"
          "...............~n"
          ".^.^.^.^.^...^.~n"
          "...............~n"
          )))])
          
  (let ([manifold
         (read-manifold in-port)])          
    (check-equal?
     (S-coord-of-manifold manifold)
     (cons 0 7))

    (check-equal?
     (splits-in-manifold manifold)
     21)
    ))

; do actual problem
;; (let ([in-port
;;        (open-input-file "test-data/input-day6-1.txt")])
;; 
;;   (let ([ceph-arith
;;          (read-cephalapod-arithmetic in-port)])          
;;     (check-equal?
;;      (solve-cephalapod-arithmetic ceph-arith)
;;      ...
;;      )))


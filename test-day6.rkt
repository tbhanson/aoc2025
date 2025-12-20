#lang racket

(require rackunit "day6.rkt")

; read input
(let ([in-port
       (open-input-string
        (format
         (string-append
          "123 328  51 64~n"
          " 45 64  387 23~n"
          "  6 98  215 314~n"
          "*   +   *   + ~n")))])
          
  (let ([ceph-arith
         (read-cephalapod-arithmetic in-port)])          
      (check-equal?
       (stream-first ceph-arith)
       (list 123 328 51 64))
    ))
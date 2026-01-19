#lang racket

(require rackunit graph "day10.rkt")


(define
  sample-input
  (format
   (string-append
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}~n"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}~n"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}~n"
    )))

(let ([in-port
       (open-input-string sample-input)])
  (let ([first-parsed-line
         (stream-first
          (read-manual-line-bits in-port))])
         
    (check-equal?
     (cadr first-parsed-line)
     "[.##.]")

    (check-equal?
     (caddr first-parsed-line)
     "(3) (1,3) (2) (2,3) (0,2) (0,1)")

    (check-equal?
     (cadddr first-parsed-line)
     "{3,5,4,7}")

  ))


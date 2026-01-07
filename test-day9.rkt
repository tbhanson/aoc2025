#lang racket

(require rackunit graph "day9.rkt")


(let ([sample-input
       (format
        (string-append
         "7,1~n"
         "11,1~n"
         "11,7~n"
         "9,7~n"
         "9,5~n"
         "2,5~n"
         "2,3~n"
         "7,3~n"
         ))])
  (let ([in-port
         (open-input-string sample-input)])
    (check-equal?
     (stream-first
      (read-positions in-port))
     (cons 7 1)))

  (let ([in-port
         (open-input-string sample-input)])
    (check-equal?
     (max-rectangle in-port)
     50)))


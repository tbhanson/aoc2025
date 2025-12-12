#lang racket

(require rackunit "day5.rkt")

; read input
(let ([in-port
       (open-input-string
        (format
         (string-append
          "3-5~n"
          "10-14~n"
          "16-20~n"
          "12-18~n~n"
          "1~n"
          "5~n"
          "8~n"
          "11~n"
          "17~n"
          "32~n")))])
          
  (let ([ID-ranges
         (read-ID-ranges in-port)]
        [IDs
        (read-IDs in-port)]
        )
    
    (check-equal?
     (stream-first ID-ranges)
     (cons 3 5))

    (check-equal?
     (stream-first IDs)
     1)
    ))

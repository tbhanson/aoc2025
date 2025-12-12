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
          
  (let ([IDs-and-ranges
         (read-IDs-and-ranges in-port)])
    (let ([ID-ranges
           (stream-filter pair? IDs-and-ranges)]
          [IDs
           (stream-filter number? IDs-and-ranges)])
          
      (check-equal?
       (stream-first ID-ranges)
       (cons 3 5))
      
      (check-equal?
       (stream-first IDs)
       1)

      (let ([fresh-IDs (set-of-fresh-IDs ID-ranges)])
        (check-true (set-member? fresh-IDs 3))
        (check-false (set-member? fresh-IDs 2))
        
        ))))

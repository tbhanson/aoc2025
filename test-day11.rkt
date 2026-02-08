#lang racket

(require rackunit graph "day11.rkt")

(require parser-tools/lex)

(define
  tiny-input
  (format
   (string-append
    "aaa: you hhh~n"
    "you: bbb ccc~n")))

(define
  sample-input
  (format
   (string-append
    "aaa: you hhh~n"
    "you: bbb ccc~n"
    "bbb: ddd eee~n"
    "ccc: ddd eee fff~n"
    "ddd: ggg~n"
    "eee: out~n"
    "fff: out~n"
    "ggg: out~n"
    "hhh: ccc fff iii~n"
    "iii: out~n"
    )))

(check-equal?
 (stream->list
  (read-graph (open-input-string tiny-input)))
 (list
  (cons "aaa" (list "you" "hhh"))
  (cons "you" (list "bbb" "ccc"))))

; their small sample problem
(let ([sample-input-port (open-input-string sample-input)])
  (let ([sample-graph-lines (read-graph sample-input-port)])
    (check-equal?
     (count-paths-from-you-to-out sample-graph-lines)
     5)))

         

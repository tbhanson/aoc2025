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


; part 1
;; (time
;;  (let ([input-port
;;         (open-input-file "test-data/input-day11-1.txt")])
;;    (let ([graph-lines (read-graph input-port)])
;;      (check-equal?
;;       (count-paths-from-you-to-out graph-lines)
;;       5))))

; part 2
      
 

(define
  part2-sample-input
  (format
   (string-append
    "svr: aaa bbb~n"
    "aaa: fft~n"
    "fft: ccc~n"
    "bbb: tty~n"
    "tty: ccc~n"
    "ccc: ddd eee~n"
    "ddd: hub~n"
    "hub: fff~n"
    "eee: dac~n"
    "dac: fff~n"
    "fff: ggg hhh~n"
    "ggg: out~n"
    "hhh: out~n"
    )))

; 2 tests based on their part 2 sample


; claude's answer is almost instantaneous (my previous best took forever)
;; (time
;;  (let ([input-port
;;         (open-input-file "test-data/input-day11-1.txt")])
;;    (let ([graph-lines (read-graph input-port)])
;;      (check-equal?
;;       (fast-part2-count graph-lines)
;;       5))))


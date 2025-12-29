#lang racket


(provide
 (contract-out
  ;part 1
  [read-positions (-> port? stream?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (let ([next-tuple
           (map string->number (string-split next-line ","))])
      (if (eof-object? next-line)
          empty-stream
          (stream-cons next-tuple (read-positions in-port))))))
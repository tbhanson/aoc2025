#lang racket


(provide
 (contract-out
  ;part 1
  [read-positions (-> port? stream?)]
  [max-rectangle (-> port? exact-nonnegative-integer?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (let ([next-pair
                 (cons
                  (car next-tuple)
                  (cadr next-tuple))])
          (stream-cons next-pair (read-positions in-port)))))))

(define (max-rectangle in-port)
  0)

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
  (let ([all-corners (read-positions in-port)])
    (for*/fold ([max-rectangle-size 0])
               ([c1 all-corners]
                [c2 all-corners]
                #:unless (equal? c1 c2))
      ;(printf "c1: ~a; c2: ~a~n" c1 c2)
      (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
            [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
        ;(printf "x-diff: ~a; y-diff: ~a~n" x-diff y-diff)
        (if (> (* x-diff y-diff) max-rectangle-size)
            (* x-diff y-diff)
            max-rectangle-size)))))
          
              
    
  
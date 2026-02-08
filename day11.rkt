#lang racket

(provide
 (contract-out
  ;part 1
  [read-graph (-> port? stream?)]
  ))

  
(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define (read-graph in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([node-links (string-split next-line ":")])
          (let ([node-name (car node-links)]
                [links (string-split (cadr node-links))])
            (let ([next-pair
                   (cons node-name links)])
              (stream-cons next-pair (read-graph in-port))))))))

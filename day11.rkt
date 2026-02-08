#lang racket

(provide
 (contract-out
  ;part 1
  [read-graph (-> port? stream?)]
  [count-paths-from-you-to-out (-> stream? exact-nonnegative-integer?)]
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

(define (count-paths-from-you-to-out graph-node-stream)
  (let ([node-hash
         (for/fold ([result (make-immutable-hash)])
                   ([next-node graph-node-stream])
           (hash-set result (car next-node) (cdr next-node)))])
      
      (define (count-from-node-named node-name)
        (let ([linked-node-names (hash-ref node-hash node-name)])
          (cond [(member "out" linked-node-names)
                 1]

              [else
               (for/fold ([sum 0])
                         ([linked-node-name linked-node-names])
                 (+ sum (count-from-node-named linked-node-name)))])))

    (count-from-node-named "you")
    ))
      
      
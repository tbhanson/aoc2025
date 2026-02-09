#lang racket

(require racket/generator)

(provide
 (contract-out
  ;part 1
  [read-graph (-> port? stream?)]
  [count-paths-from-you-to-out (-> stream? exact-nonnegative-integer?)]
  [find-paths-from-node-to-out (-> stream? string? stream?)]
  [part2-path-count (-> stream? exact-nonnegative-integer?)]
  [generator-of-paths-from-svr-to-out  (-> stream? generator?)]
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
      

(define (find-paths-from-node-to-out graph-node-stream node-name)
  (let ([node-hash
         (for/fold ([result (make-immutable-hash)])
                   ([next-node graph-node-stream])
           (hash-set result (car next-node) (cdr next-node)))])
      
    (define (paths-to-out-from-node-named node-name path-to-here)
      (let ([linked-node-names (hash-ref node-hash node-name)])
        (cond [(member "out" linked-node-names)
               (stream-cons
                (cons "out"
                      path-to-here)
                empty-stream)]

              [else
               (for/fold ([paths-from-here empty-stream])
                         ([linked-node-name linked-node-names])
                 (stream-append
                  paths-from-here
                  (paths-to-out-from-node-named
                   linked-node-name
                   (cons linked-node-name path-to-here))))])))

    (stream-map reverse (paths-to-out-from-node-named node-name (list node-name)))))

(define (generator-of-paths-from-svr-to-out graph-node-stream)
  (let ([node-hash
         (for/fold ([result (make-immutable-hash)])
                   ([next-node graph-node-stream])
           (hash-set result (car next-node) (cdr next-node)))])
    
    (generator ()
      (define (paths-to-out-from-node-named node-name path-to-here)
        (let ([linked-node-names (hash-ref node-hash node-name)])
          (cond [(member "out" linked-node-names)
                 (yield (reverse (cons "out" path-to-here)))]
                [else
                 (let ([new-path (cons node-name path-to-here)])
                   (for ([linked-node-name linked-node-names])
                     (paths-to-out-from-node-named
                      linked-node-name
                      new-path)))])))
      
      (paths-to-out-from-node-named "svr" '("svr")))))

(define (part2-path-count graph-node-stream)
  (stream-length
   (stream-filter
    (lambda (path)
      (and (member "dac" path)
           (member "fft" path)))
    (find-paths-from-node-to-out graph-node-stream "svr"))))

;; (define (part2-generated-path-count graph-node-stream)
;;   (let ([gen (generator-of-paths-from-svr-to-out graph-node-stream)])
;;     (gen "svr" '())))
   
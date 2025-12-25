#lang racket

(require parser-tools/lex)
(require math/array)


(provide
 (contract-out
  ;part 1
  [read-manifold (-> port? vector?)]
  [S-coord-of-manifold (-> vector? pair?)]
  ))

(define (read-manifold in-port)
  (define (read-lines a-port)
    (let ([next-line (read-line a-port)])
      (if (eof-object? next-line)
          empty-stream
          (stream-cons next-line (read-lines a-port)))))

  (for/vector ([line (read-lines in-port)])
    (list->vector
     (string->list line))))

(define (unique a-list)
  (cond [(= (length a-list) 1)
         (car a-list)]
        [else
         (error
          (format "unique demands precisely 1 element, but got ~a" a-list))]))
         
(define (S-coord-of-manifold manifold)
  (let ([row-count (vector-length manifold)]
        [col-count (vector-length (vector-ref manifold 0))])
    (unique
     (for*/fold ([S-coords '()])
       ([row (in-range row-count)]
        [col (in-range col-count)])
       (if (char=?
            (vector-ref (vector-ref manifold row) col)
            #\S)
           (cons
            (cons row col)
            S-coords)
           S-coords)))))

  

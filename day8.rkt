#lang racket


(provide
 (contract-out
  ;part 1
  [read-positions (-> port? stream?)]
  [closest-pair (-> stream? set?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (stream-cons next-tuple (read-positions in-port))))))

(define (distance t1 t2)
  (define (square x)
    (* x x))
  (for/fold ([sum 0])
            ([c1 t1]
             [c2 t2])
    (+ sum (square (- c2 c1)))))

(define (closest-pair tuples)
  (let ([first-tuple (stream-first tuples)]
        [second-tuple (stream-first (stream-rest tuples))])
    (let ([first-pair-distance (distance first-tuple second-tuple)])
      (let-values ([(closest how-close)
                    (for*/fold ([closest-pair
                                 (list first-tuple second-tuple)]
                                [closest-distance first-pair-distance])
                               ([t1 tuples]
                                [t2 tuples]
                                #:unless (equal? t1 t2))
                      (let ([next-distance (distance t1 t2)])
                        (if (< next-distance closest-distance)
                            (values (list t1 t2) next-distance)
                            (values closest-pair closest-distance))))])
        (list->set closest)))))

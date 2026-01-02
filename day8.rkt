#lang racket

(require graph)

(struct numbered-positions
  (by-number by-position connections)
  #:prefab
  )

(provide
 (contract-out
  ; struct automatics
  [numbered-positions? (-> any/c boolean?)]
  [numbered-positions (-> hash? hash? graph? numbered-positions?)]
  [numbered-positions-by-number (-> numbered-positions? hash? )]
  [numbered-positions-by-position (-> numbered-positions? hash? )]
  [numbered-positions-connections (-> numbered-positions? graph? )]
  
  ;part 1
  [read-positions (-> port? stream?)]
  [read-numbered-positions (-> port? numbered-positions?)]
  [closest-pair (-> stream? set?)]
  [connect-closest-unconnected (-> numbered-positions? numbered-positions?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (stream-cons next-tuple (read-positions in-port))))))

(define (read-numbered-positions in-port)
  (let ([positions (read-positions in-port)])
    (let-values ([(positions-by-number
                   numbers-by-positions)
                  (for/fold ([by-number (make-immutable-hash)]
                             [by-position (make-immutable-hash)])
                            ([counter (in-naturals 1)]
                             [next-position positions])
                    (values
                     (hash-set by-number counter next-position)
                     (hash-set by-position next-position counter)))])
      (numbered-positions positions-by-number numbers-by-positions))))
       
      

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

(define (connect-closest-unconnected world)
  '())
  
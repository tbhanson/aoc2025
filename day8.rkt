#lang racket

(require graph)

(struct point-world
  (by-number by-position connections)
  #:prefab
  )

(provide
 (contract-out
  ; struct automatics
  [point-world? (-> any/c boolean?)]
  [point-world (-> hash? hash? graph? point-world?)]
  [point-world-by-number (-> point-world? hash? )]
  [point-world-by-position (-> point-world? hash? )]
  [point-world-connections (-> point-world? graph? )]
  
  ;part 1
  [read-positions (-> port? stream?)]
  [read-point-world (-> port? point-world?)]
  [closest-pair (-> stream? set?)]
  [connect-closest-unconnected (-> point-world? point-world?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (stream-cons next-tuple (read-positions in-port))))))

(define (read-point-world in-port)
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
      (point-world positions-by-number numbers-by-positions (undirected-graph '())))))
       
      

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

; we'll assume there are at least 2 points, because there always are in these examples
(define (connect-closest-unconnected world)
  (let ([points-by-number (point-world-by-number world)]
        [connections (point-world-connections world)])
;;     (printf "(connect-closest-unconnected world)~n")
;;     (printf "  connections: ~a~n" (get-edges connections))
    
    (let ([first-tuple (hash-ref points-by-number 1)]
          [second-tuple (hash-ref points-by-number 2)])
           
      (let ([first-pair-distance (distance first-tuple second-tuple)])
        (let-values ([(closest how-close)
                      (for*/fold ([closest-pair
                                   (list first-tuple second-tuple)]
                                  [closest-distance first-pair-distance])
                                 ([i1 (hash-keys points-by-number)]
                                  [i2 (hash-keys points-by-number)]
                                  #:unless (equal? i1 i2)
                                  #:unless
                                  (let ([t1 (hash-ref points-by-number i1)]
                                        [t2 (hash-ref points-by-number i2)])
                                    (has-edge? connections t1 t2)))
                                  
                        (let ([t1 (hash-ref points-by-number i1)]
                              [t2 (hash-ref points-by-number i2)])
                          (let ([next-distance (distance t1 t2)])
                            (if (< next-distance closest-distance)
                                (begin
                                  ;(printf "  new closest points: ~a and ~a~n" t1 t2)
                                  (values (list t1 t2) next-distance))
                                (values closest-pair closest-distance)))))])
          (let ([new-v1 (car closest)]
                [new-v2 (cadr closest)])
            (begin
              (add-edge! connections new-v1 new-v2)
              world)))))))
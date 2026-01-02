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
  [connected-to-vertex (-> graph? list? set?)]
  [connected-sub-graphs (-> graph? list?)]
  [their-funny-product-after-N-iterations (-> point-world? exact-nonnegative-integer? exact-nonnegative-integer?)]
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
    ;(printf "(connect-closest-unconnected world)~n")
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
              ;(printf "  connecting ~a to ~a~n" new-v1 new-v2)
              (add-edge! connections new-v1 new-v2)
              world)))))))

(define (connected-to-vertex a-graph a-vertex)
  (define (iter so-far still-to-check)
    (if (set-empty? still-to-check)
        so-far
        (let ([next-to-check (set-first still-to-check)])
          (if (set-member? so-far next-to-check)
              (iter so-far (set-rest still-to-check))
              (let ([new-frontier (get-neighbors a-graph next-to-check)])
                (iter
                 (set-add so-far next-to-check)
                 (set-union (set-rest still-to-check) (list->set new-frontier))))))))
  
  ;(printf "(connected-to-vertex a-graph ~a)~n" a-vertex)
  (let ([result
         (iter (set) (set a-vertex))])
    ;(printf " -> ~a~n" result)
    result))
    
  
; return list of lists of connected points
(define (connected-sub-graphs world-graph)
  (define (iter subgraphs-so-far vertices-checked vertices-remaining-to-check)
    (if (set-empty? vertices-remaining-to-check)
        subgraphs-so-far
        (let ([next-vertex-to-check (set-first vertices-remaining-to-check)])
          (let ([next-sub-graph
                 (connected-to-vertex world-graph next-vertex-to-check)])
            (iter
             (cons next-sub-graph subgraphs-so-far)
             (set-union vertices-checked next-sub-graph)
             (set-subtract vertices-remaining-to-check next-sub-graph))))))

  (iter '() (set) (list->set (get-vertices world-graph))))
               
   

(define (their-funny-product-after-N-iterations world N)
  ;(printf "(their-funny-product-after-N-iterations world ~a)~n" N)
  (let ([world-after
         (for/fold ([current-world world])
                   ([i (in-range N)])
           (connect-closest-unconnected current-world))])
    (let ([sub-graphs (connected-sub-graphs (point-world-connections world-after))])
      ;(printf " sub-graphs: ~a~n" (pretty-print sub-graphs))
      (let ([sizes-desc
             (sort (map set-count sub-graphs) >=)])
        ;(printf " sizes-desc: ~a~n" sizes-desc)
        (* (car sizes-desc)
           (cadr sizes-desc)
           (caddr sizes-desc))))))
           

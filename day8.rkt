#lang racket

(require graph)

(struct point-world
  (by-number by-position pairs-by-distance sorted-distances connections last-pair-connected)
  #:prefab
  )

(provide
 (contract-out
  ; struct automatics
  [point-world? (-> any/c boolean?)]
  [point-world (-> hash? hash? hash? list? graph? pair? point-world?)]
  [point-world-by-number (-> point-world? hash? )]
  [point-world-by-position (-> point-world? hash? )]
  [point-world-pairs-by-distance (-> point-world? hash? )]
  [point-world-sorted-distances (-> point-world? list? )]
  [point-world-connections (-> point-world? graph? )]
  [point-world-last-pair-connected  (-> point-world? pair? )]
  
  ;part 1
  [read-positions (-> port? stream?)]
  [read-point-world (-> port? point-world?)]
  [connect-closest-unconnected (-> point-world? point-world?)]
  [connected-to-vertex (-> graph? list? set?)]
  [connected-sub-graphs (-> graph? list?)]
  [their-funny-product-after-N-iterations (-> point-world? exact-nonnegative-integer? exact-nonnegative-integer?)]

  ; part 2
  [their-funny-product-after-all-connected (-> point-world? exact-nonnegative-integer?)]
  [world-invariant-checks (-> point-world? void?)]
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
  
  (let ([result
         (for/fold ([sum 0])
                   ([c1 t1]
                    [c2 t2])
           (+ sum (square (- c2 c1))))])
    result))

(define (merge-hashes h1 h2)
  (for/fold ([result h1])
            ([add-key (hash-keys h2)])
    (hash-set result add-key (hash-ref h2 add-key))))
  
(define (read-point-world in-port)
  (let ([positions (read-positions in-port)])
    (let-values ([(positions-by-number
                   numbers-by-positions
                   min-pos-num
                   max-pos-num)
                  (for/fold ([by-number (make-immutable-hash)]
                             [by-position (make-immutable-hash)]
                             [min-idx 999999]
                             [max-idx -1])
                            ([counter (in-naturals 1)]
                             [next-position positions])
                    (values
                     (hash-set by-number counter next-position)
                     (hash-set by-position next-position counter)
                     (if (< counter min-idx) counter min-idx)
                     (if (> counter max-idx) counter max-idx)))])
      (let ([point-pairs-by-distance-between
             (for/fold ([point-pairs-by-distance (make-immutable-hash)])
                       ([i1 (in-range min-pos-num max-pos-num)]) ; sic: up to N-1
               (let ([t1 (hash-ref positions-by-number i1)])
                 (merge-hashes
                  point-pairs-by-distance
                  (for/fold ([one-row-point-pairs-by-distance (make-immutable-hash)])
                            ([i2 (in-range (+ i1 1) (+ max-pos-num 1))]) ; sic: up to N
                    (let ([t2 (hash-ref positions-by-number i2)])
                      (hash-set
                       one-row-point-pairs-by-distance
                       (distance t1 t2)
                       (cons t1 t2)))))))])

        (let ([sorted-distances
               (sort
                (hash-keys point-pairs-by-distance-between)
                <)])

          (let ([result
                 (point-world positions-by-number numbers-by-positions point-pairs-by-distance-between sorted-distances (undirected-graph '()) #f)])
            (world-invariant-checks result)
            result))))))
       
      


; we'll assume there are at least 2 points, because there always are in these examples
(define (connect-closest-unconnected world)
  (let ([sorted-distances (point-world-sorted-distances world)])
    (let ([shortest-remaining-distance (car sorted-distances)]
          [new-remaining-distances (cdr sorted-distances)]
          [connections (point-world-connections world)])
      (printf "(connect-closest-unconnected world)~n")
      ;(printf "  connections: ~a~n" (get-edges connections))
      (let ([new-pair-to-connect
             (hash-ref (point-world-pairs-by-distance world) shortest-remaining-distance)])
        (let ([new-v1 (car new-pair-to-connect)]
              [new-v2 (cdr new-pair-to-connect)])
          (begin
            (printf "  connecting ~a to ~a~n" new-v1 new-v2)
            (add-edge! connections new-v1 new-v2)
            (point-world
             (point-world-by-number world)
             (point-world-by-position world)
             (point-world-pairs-by-distance world)
             new-remaining-distances
             connections
             new-pair-to-connect)))))))
              
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

(define (their-funny-product-after-all-connected world)
  (define (iter world-so-far)
    (if (=
         (length
          (connected-sub-graphs
           (point-world-connections world-so-far)))
         1)
        (let ([x1
               (car (car (point-world-last-pair-connected world-so-far)))]
              [x2
               (car (cdr (point-world-last-pair-connected world-so-far)))])
          (* x1 x2))
        (iter
         (connect-closest-unconnected world-so-far))))

  (iter world))
              

(define (world-invariant-checks world)
  (define (assert pred anError)
    (if (not pred) 
        (error anError)
        #t))
  
  (assert
   (= (length (hash-keys (point-world-by-number world)))
      (length (hash-keys (point-world-by-position world))))
   (format "world should have the same number of positions (~a) and number of same (~a)"
           (length (hash-keys (point-world-by-number world)))
           (length (hash-keys (point-world-by-position world))))
   )
  )
;;   [point-world-pairs-by-distance (-> point-world? hash? )]
;;   [point-world-sorted-distances (-> point-world? list? )]
;;   [point-world-connections (-> point-world? graph? )]
;;   [point-world-last-pair-connected  (-> point-world? pair? )]


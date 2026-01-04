#lang racket

(require graph)

(struct point-world
  (by-number by-position pair-lists-by-distance sorted-distances connections last-pair-connected)
  #:prefab
  )

(provide
 (contract-out
  ; struct automatics
  [point-world? (-> any/c boolean?)]
  [point-world (-> hash? hash? hash? list? graph? pair? point-world?)]
  [point-world-by-number (-> point-world? hash? )]
  [point-world-by-position (-> point-world? hash? )]
  [point-world-pair-lists-by-distance (-> point-world? hash? )] ; hash of lists of pairs by distance (!)
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

; there might be collisions!
(define (merge-hashes h1 h2)
  (for/fold ([result h1])
            ([add-key (hash-keys h2)])
    (let ([h2-part (hash-ref h2 add-key)]
          [h1-part (hash-ref h1 add-key '())])
          
      (hash-set result add-key (append h1-part h2-part)))))
  
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
      (let ([point-pair-lists-by-distance-between
             (for/fold ([point-pairs-by-distance (make-immutable-hash)])
                       ([i1 (in-range min-pos-num max-pos-num)]) ; sic: up to N-1
               (let ([t1 (hash-ref positions-by-number i1)])
                 (merge-hashes
                  point-pairs-by-distance
                  (for/fold ([one-row-point-pairs-by-distance (make-immutable-hash)])
                            ([i2 (in-range (+ i1 1) (+ max-pos-num 1))]) ; sic: up to N
                    (let ([t2 (hash-ref positions-by-number i2)])
                      (let ([d-t1-t2 (distance t1 t2)])
                        (hash-set
                         one-row-point-pairs-by-distance
                         d-t1-t2
                         (cons
                          (cons t1 t2)
                          (hash-ref one-row-point-pairs-by-distance d-t1-t2 '()))
                         )))))))])
        
        (let ([sorted-distances
               (sort
                (hash-keys point-pair-lists-by-distance-between)
                <)])
          
          (let ([result
                 (point-world positions-by-number numbers-by-positions point-pair-lists-by-distance-between sorted-distances (undirected-graph '()) #f)])
            (world-invariant-checks result)
            result))))))
       
      


; we'll assume there are at least 2 points, because there always are in these examples
(define (connect-closest-unconnected world)
  ;(printf "(connect-closest-unconnected world)~n")
  (let ([sorted-distances (point-world-sorted-distances world)])
    (let ([shortest-remaining-distance (car sorted-distances)]
          [new-remaining-distances (cdr sorted-distances)]
          [connections (point-world-connections world)])
      ; here we assume there is always only one pair per distance (though we've allowed for several just in case -- an invariant checks for this)
      (let ([new-pair-to-connect
             (car
              (hash-ref (point-world-pair-lists-by-distance world) shortest-remaining-distance))])
        ;(printf "  new-pair-to-connect: ~a~n" new-pair-to-connect)
        
        (let ([new-v1 (car new-pair-to-connect)]
              [new-v2 (cdr new-pair-to-connect)])
          (begin
            ;(printf "  connecting ~a to ~a~n" new-v1 new-v2)
            (add-edge! connections new-v1 new-v2)
            (let ([result
                   (point-world
                    (point-world-by-number world)
                    (point-world-by-position world)
                    (point-world-pair-lists-by-distance world)
                    new-remaining-distances
                    connections
                    new-pair-to-connect)])
              (world-invariant-checks result)
              result)))))))
              
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
  (define (all-connected world-in-progress)
    ;(printf "(all-connected world-in-progress)~n")
    (let ([sub-graphs
           (connected-sub-graphs 
            (point-world-connections world-in-progress))]
          [count-of-points (length (hash-keys (point-world-by-number world)))])
      ;(printf " sub-graphs: ~a~n" sub-graphs)
      (and
       (= (length sub-graphs) 1)
       (=
        (set-count
         (car sub-graphs))
        count-of-points))))
  
  (define (iter world-so-far)
    (if (all-connected world-so-far)
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

  (let ([pair-lists-by-distance (point-world-pair-lists-by-distance world)])
    (let ([count-how-many-pairs-each-distance
           (for/fold ([result (make-immutable-hash)])
                     ([next-distance
                       (hash-keys pair-lists-by-distance)])
             (let ([pair-count-this-distance
                    (length (hash-ref pair-lists-by-distance next-distance))])
               (let ([prior-count-count
                      (hash-ref result pair-count-this-distance 0)])
                 (hash-set result pair-count-this-distance (+ prior-count-count 1)))))])
      ;(printf "count-how-many-pairs-each-distance: ~a~n" count-how-many-pairs-each-distance)
      (assert
       (equal? '(1)
               (hash-keys count-how-many-pairs-each-distance))
       (format "unexpectedly more than one pair at a distance: (count-how-many-pairs-each-distance: ~a)~n" count-how-many-pairs-each-distance))))
      
                   
  ;;   ; this one is subtle: the number of distances between pairs
  ;;   (assert
  ;;    (= (length (hash-keys (point-world-pairs-by-distance world)))
  ;;       (length (point-world-sorted-distances world)))
  ;;    (format "world should have the same number of distances between pairs (~a) and number of sorted distances (~a)"
  ;;            (length (hash-keys (point-world-pairs-by-distance world)))
  ;;            (length (point-world-sorted-distances world)))
  ;;    )

  ; all vertices must be points from our world
  (assert
   (subset?
    (list->set
     (get-vertices (point-world-connections world)))
    (list->set
     (hash-keys (point-world-by-position world))))
   (format "vertices in world's connectivity graph must all be points in the world (these aren't: ~a)"
           (set-subtract
            (list->set
             (get-vertices (point-world-connections world)))
            (list->set
             (hash-keys (point-world-by-position world))))))

;;   (printf " sizes of (connected-sub-graphs (point-world-connections world)): ~a~n"
;;           (sort
;;            (map set-count (connected-sub-graphs (point-world-connections world)))
;;            >=)
;;           )
;; 
;;   (printf " vertex count in (point-world-connections world): ~a~n"
;;           (length (get-vertices (point-world-connections world))))
;; 
;;   (printf " edge count in (point-world-connections world): ~a~n"
;;           (length (get-edges (point-world-connections world))))

  )
;;   [point-world-pairs-by-distance (-> point-world? hash? )]
;;   [point-world-sorted-distances (-> point-world? list? )]
;;   [point-world-connections (-> point-world? graph? )]
;;   [point-world-last-pair-connected  (-> point-world? pair? )]


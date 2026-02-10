#lang racket

(require racket/generator)

(provide
 (contract-out
  ;part 1
  [read-graph (-> port? stream?)]
  [count-paths-from-you-to-out (-> stream? exact-nonnegative-integer?)]
  [find-paths-from-node-to-out (-> stream? string? stream?)]
  ;part 2
  [part2-path-count (-> stream? exact-nonnegative-integer?)]
  [generator-of-paths-from-svr-to-out  (-> stream? generator?)]
  [part2-generated-path-count (-> stream? exact-nonnegative-integer?)]
  ; part 2, second pass
  [linked-nodes-hash (-> stream? hash?)]
  [predecessor-nodes-hash (-> hash? hash?)]
  ;;   
  [find-predecessors-of-node-named (-> hash? string? set?)]
  [find-all-predecessors-of-node-named (-> hash? string? set?)]

  [generator-of-paths-from-svr-to-out-via-dac-and-fft (-> stream? generator?)]
  [new-part2-generated-path-count (-> stream? exact-nonnegative-integer?)]

  ; asked claude:
  [fast-part2-count (-> stream? exact-nonnegative-integer?)]
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

(define (linked-nodes-hash graph-node-stream)
  (for/fold ([result (make-immutable-hash)])
            ([next-node graph-node-stream])
    (hash-set result (car next-node) (list->set (cdr next-node)))))

(define (predecessor-nodes-hash nodes-hash)
  ;(printf "(predecessor-nodes-hash ~a)~n" nodes-hash)
  (let ([result-hash
         (for/fold ([result (make-immutable-hash)])
                   ([next-linking-node-name (hash-keys nodes-hash)])
           (for/fold ([new-result result])
                     ([linked-to-node-name (hash-ref nodes-hash next-linking-node-name)])
             (let ([linked-from-so-far
                    (hash-ref new-result linked-to-node-name (set))])
               (hash-set new-result linked-to-node-name (set-add linked-from-so-far next-linking-node-name)))))])
    ;(printf "--> ~a~n" result-hash)
    result-hash))

(define (find-predecessors-of-node-named linked-from-hash node-name)
  (hash-ref linked-from-hash node-name (set)))

(define (find-all-predecessors-of-node-named linked-from-hash node-name)
  (define (iter so-far to-explore)
    (if (set-empty? to-explore)
        so-far
        (let ([next-one-to-explore (set-first to-explore)]
              [remaining-ones-to-maybe-explore (set-rest to-explore)])
          (let ([all-new-to-explore
                 (set-subtract
                  (set-union
                   (find-predecessors-of-node-named linked-from-hash next-one-to-explore)
                   remaining-ones-to-maybe-explore)
                  so-far)]
                [new-so-far
                 (set-add so-far next-one-to-explore)])
            (iter new-so-far all-new-to-explore)))))

  (set-remove
   (iter (set) (set node-name))
   node-name))
    
  
(define (count-paths-from-you-to-out graph-node-stream)
  ;(printf "(count-paths-from-you-to-out <graph-node-stream>)~n")
  
  (let ([node-hash (linked-nodes-hash graph-node-stream)])
    ;(printf " node-hash: ~a~n" node-hash)
      
    (define (count-from-node-named node-name)
      ;(printf "(count-from-node-named ~a)~n" node-name)
      (let ([linked-node-names (hash-ref node-hash node-name)])
        ;(printf " linked-node-names: ~a~n" linked-node-names)
        (cond [(set-member? linked-node-names "out")
               1]

              [else
               (for/fold ([sum 0])
                         ([linked-node-name linked-node-names])
                 (+ sum (count-from-node-named linked-node-name)))])))

    (count-from-node-named "you")
    ))
      

(define (find-paths-from-node-to-out graph-node-stream node-name)
  (let ([node-hash (linked-nodes-hash graph-node-stream)])
      
    (define (paths-to-out-from-node-named node-name path-to-here)
      (let ([linked-node-names (hash-ref node-hash node-name)])
        (cond [(set-member? linked-node-names "out")
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
  ;(printf "(generator-of-paths-from-svr-to-out ...)~n")
  (let ([node-hash (linked-nodes-hash graph-node-stream)])
    
    (generator ()
               (define (paths-to-out-from-node-named node-name path-to-here)
                 ;(printf "  (paths-to-out-from-node-named ~a ~a)~n" node-name path-to-here)
                 (let ([linked-node-names (hash-ref node-hash node-name)])
                   (cond [(set-member? linked-node-names "out")
                          (yield (reverse (cons "out" path-to-here)))]
                
                         [else
                          (for ([linked-node-name linked-node-names])
                            (paths-to-out-from-node-named
                             linked-node-name
                             (cons linked-node-name path-to-here)))])))
      
               (paths-to-out-from-node-named "svr" '("svr")))))

(define (part2-path-count graph-node-stream)
  (stream-length
   (stream-filter
    (lambda (path)
      (and (member "dac" path)
           (member "fft" path)))
    (find-paths-from-node-to-out graph-node-stream "svr"))))

(define (part2-generated-path-count graph-node-stream)
  (let ([gen (generator-of-paths-from-svr-to-out graph-node-stream)])
    (let-values ([(sum c-dac c-fft)
                  (for/fold ([sum 0]
                             [saw-dac-count 0]
                             [saw-fft-count 0])
                            ([next-path (in-producer gen (void))] ; void as sentinel
                             [counter (in-naturals 1)])
                    (let ([new-sum
                           (if (and (member "dac" next-path)
                                    (member "fft" next-path))
                               (+ sum 1)
                               sum)]
                          [new-dac-count
                           (if (member "dac" next-path)
                               (+ saw-dac-count 1)
                               saw-dac-count)]
                          [new-fft-count
                           (if (member "fft" next-path)
                               (+ saw-fft-count 1)
                               saw-fft-count)])
                                                    
                      (cond
                        [(= 0 (remainder counter 100000))
                         (printf ".")]
                        [(= 1 (remainder sum 10))
                         (printf "s")]
                        [(= 17 (remainder counter 500000))
                         (printf "counter: ~a; [process time ~as] saw-dac: ~a; saw-fft: ~a; sum: ~a; next-path: ~a~n"
                                 counter (/ (current-process-milliseconds) 1000.0) new-dac-count new-fft-count sum next-path)]
                        )
                      (values new-sum new-dac-count new-fft-count)
                      ))])
      sum)))

(define (generator-of-paths-from-svr-to-out-via-dac-and-fft graph-node-stream)
  ;(printf "(generator-of-paths-from-svr-to-out-via-dac-and-fft <graph-node-stream>)~n")
  (let ([linked-hash (linked-nodes-hash graph-node-stream)])
    (let ([linked-from-hash
           (predecessor-nodes-hash linked-hash)])
      (let ([pred-out (find-all-predecessors-of-node-named linked-from-hash "out")]
            [pred-dac (find-all-predecessors-of-node-named linked-from-hash "dac")]
            [pred-fft (find-all-predecessors-of-node-named linked-from-hash "fft")])
        (let ([pred-relevant (set-intersect pred-out pred-dac pred-fft)])
 
    
          (generator ()
                     (define (paths-to-out-from-node-named node-name path-to-here)
                       ;(printf "  (paths-to-out-from-node-named ~a ~a)~n" node-name path-to-here)
                       (let ([linked-node-names (hash-ref linked-hash node-name)])
                         (let ([relevant-linked-node-names
                                (set-intersect linked-node-names pred-relevant)])
                           (cond [(set-member? linked-node-names "out")
                                  (yield (reverse (cons "out" path-to-here)))]
                               
                                 [else
                                  (for ([linked-node-name linked-node-names])
                                    (paths-to-out-from-node-named
                                     linked-node-name
                                     (cons linked-node-name path-to-here)))]))))
      
                     (paths-to-out-from-node-named "svr" '("svr"))))))))

  ;; (define (new-part2-generated-path-count graph-node-stream)
  ;;   (let ([gen (generator-of-paths-from-svr-to-out-via-dac-and-fft graph-node-stream)])
  ;;     (for/fold ([sum 0])
  ;;               ([next-path (in-producer gen (void))] ; void as sentinel
  ;;                [counter (in-naturals 1)])
  ;;       sum)))

  (define (new-part2-generated-path-count graph-node-stream)
    (let ([gen (generator-of-paths-from-svr-to-out-via-dac-and-fft graph-node-stream)])
      (let-values ([(sum c-dac c-fft)
                    (for/fold ([sum 0]
                               [saw-dac-count 0]
                               [saw-fft-count 0])
                              ([next-path (in-producer gen (void))] ; void as sentinel
                               [counter (in-naturals 1)])
                      (let ([new-sum
                             (if (and (member "dac" next-path)
                                      (member "fft" next-path))
                                 (+ sum 1)
                                 sum)]
                            [new-dac-count
                             (if (member "dac" next-path)
                                 (+ saw-dac-count 1)
                                 saw-dac-count)]
                            [new-fft-count
                             (if (member "fft" next-path)
                                 (+ saw-fft-count 1)
                                 saw-fft-count)])
                                                    
                        (if (= 0 (remainder counter 100000))
                            (printf ".")
                            (cond [(= 1 (remainder sum 100000))
                                   (printf "s")]
                                
                                  [(= 17 (remainder counter 500000))
                                   (printf "counter: ~a; [process time ~as] saw-dac: ~a; saw-fft: ~a; sum: ~a; next-path: ~a~n"
                                           counter (/ (current-process-milliseconds) 1000.0) new-dac-count new-fft-count sum next-path)]
                                  ))
                        (values new-sum new-dac-count new-fft-count)
                        ))])
        sum)))

(define (count-paths-with-memo node-hash from-node to-node memo)
  (let ([key (cons from-node to-node)])
    (cond
      [(hash-has-key? memo key)
       (values (hash-ref memo key) memo)]
      [(equal? from-node to-node)
       (values 1 memo)]
      [else
       (let ([linked-nodes (hash-ref node-hash from-node (set))])
         (let-values ([(total new-memo)
                       (for/fold ([sum 0]
                                  [current-memo memo])
                                 ([next-node linked-nodes])
                         (let-values ([(count updated-memo)
                                       (count-paths-with-memo 
                                        node-hash next-node to-node current-memo)])
                           (values (+ sum count) updated-memo)))])
           (values total (hash-set new-memo key total))))])))

(define (fast-part2-count graph-node-stream)
  (let ([node-hash (linked-nodes-hash graph-node-stream)])
    ;; Count paths: svr -> dac -> fft -> out
    (let-values ([(count1-dac memo1) 
                  (count-paths-with-memo node-hash "svr" "dac" (hash))])
      (let-values ([(count2-fft memo2)
                    (count-paths-with-memo node-hash "dac" "fft" memo1)])
        (let-values ([(count3-out memo3)
                      (count-paths-with-memo node-hash "fft" "out" memo2)])
          (let ([path1-total (* count1-dac count2-fft count3-out)])
            
            ;; Count paths: svr -> fft -> dac -> out
            (let-values ([(count1-fft memo4)
                          (count-paths-with-memo node-hash "svr" "fft" memo3)])
              (let-values ([(count2-dac memo5)
                            (count-paths-with-memo node-hash "fft" "dac" memo4)])
                (let-values ([(count3-out memo6)
                              (count-paths-with-memo node-hash "dac" "out" memo5)])
                  (let ([path2-total (* count1-fft count2-dac count3-out)])
                    ;; Return sum (might need to handle overlaps depending on graph)
                    (+ path1-total path2-total)))))))))))
  
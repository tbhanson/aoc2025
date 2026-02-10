#lang racket

(require rackunit graph "day11.rkt")

(require parser-tools/lex)

(define
  tiny-input
  (format
   (string-append
    "aaa: you hhh~n"
    "you: bbb ccc~n")))

(define
  sample-input
  (format
   (string-append
    "aaa: you hhh~n"
    "you: bbb ccc~n"
    "bbb: ddd eee~n"
    "ccc: ddd eee fff~n"
    "ddd: ggg~n"
    "eee: out~n"
    "fff: out~n"
    "ggg: out~n"
    "hhh: ccc fff iii~n"
    "iii: out~n"
    )))

(check-equal?
 (stream->list
  (read-graph (open-input-string tiny-input)))
 (list
  (cons "aaa" (list "you" "hhh"))
  (cons "you" (list "bbb" "ccc"))))

; their small sample problem
(let ([sample-input-port (open-input-string sample-input)])
  (let ([sample-graph-lines (read-graph sample-input-port)])
    (check-equal?
     (count-paths-from-you-to-out sample-graph-lines)
     5)))


; part 1
;; (time
;;  (let ([input-port
;;         (open-input-file "test-data/input-day11-1.txt")])
;;    (let ([graph-lines (read-graph input-port)])
;;      (check-equal?
;;       (count-paths-from-you-to-out graph-lines)
;;       5))))

; part 2
(let ([input-port
       (open-input-string tiny-input)])
  (let ([graph-lines (read-graph input-port)])
    (let ([linked-hash (linked-nodes-hash graph-lines)])
      (check-equal?
       linked-hash
       (hash "aaa" (set "you" "hhh") "you" (set "bbb" "ccc"))
       )
      (let ([linked-from-hash
             (predecessor-nodes-hash linked-hash)])
        (check-equal?
         linked-from-hash
         (hash "you" (set "aaa") "hhh" (set "aaa") "bbb" (set "you") "ccc" (set "you")))

        (let ([precedes-you
               (find-predecessors-of-node-named linked-from-hash "you")])
          (check-equal? precedes-you (set "aaa"))
          )))))
      
(let ([input-port
       (open-input-string sample-input)])
  (let ([graph-lines (read-graph input-port)])
    (let ([linked-hash (linked-nodes-hash graph-lines)])
      (let ([linked-from-hash
             (predecessor-nodes-hash linked-hash)])
        (let ([precedes-you
               (find-predecessors-of-node-named linked-from-hash "you")])
          (check-equal? precedes-you (set "aaa"))
          
          (check-equal?
           (find-predecessors-of-node-named linked-from-hash "out")
           (list->set (string-split "eee,fff,ggg,iii" ",")))

          (check-equal?
           (find-all-predecessors-of-node-named linked-from-hash "bbb")
           (list->set (string-split "you,aaa" ",")))

          (check-equal?
           (set-count
            (find-all-predecessors-of-node-named linked-from-hash "out"))
           10)
             
          )))))
      
 

(define
  part2-sample-input
  (format
   (string-append
    "svr: aaa bbb~n"
    "aaa: fft~n"
    "fft: ccc~n"
    "bbb: tty~n"
    "tty: ccc~n"
    "ccc: ddd eee~n"
    "ddd: hub~n"
    "hub: fff~n"
    "eee: dac~n"
    "dac: fff~n"
    "fff: ggg hhh~n"
    "ggg: out~n"
    "hhh: out~n"
    )))

; 2 tests based on their part 2 sample
(let ([part2-sample-input-port (open-input-string part2-sample-input)])
  (let ([part2-sample-graph-lines (read-graph part2-sample-input-port)])
    (check-true
     (set-member?
      (list->set
       (stream->list
        (find-paths-from-node-to-out part2-sample-graph-lines "svr")))
      (string-split "svr,aaa,fft,ccc,ddd,hub,fff,ggg,out" ",")))))


(let ([part2-sample-input-port (open-input-string part2-sample-input)])
  (let ([part2-sample-graph-lines (read-graph part2-sample-input-port)])
    (check-equal?
     (part2-path-count part2-sample-graph-lines)
     2)))

;;  ; we're using sets now, so order is not deterministic; next test covers
;; (let ([part2-sample-input-port (open-input-string part2-sample-input)])
;;   (let ([part2-sample-graph-lines (read-graph part2-sample-input-port)])
;;     (let ([gen (generator-of-paths-from-svr-to-out part2-sample-graph-lines)])
;;       (check-equal?
;;        (gen)
;;        (string-split "svr,aaa,fft,ccc,ddd,hub,fff,ggg,out" ",")))))

(let ([part2-sample-input-port (open-input-string part2-sample-input)])
  (let ([part2-sample-graph-lines (read-graph part2-sample-input-port)])
    (check-equal?
     (part2-generated-path-count part2-sample-graph-lines)
     2)))

;(printf "------------ before tackling whole problem --------~n")

;; a generator attempt runs seemingly forever
;; (time
;;  (let ([input-port
;;         (open-input-file "test-data/input-day11-1.txt")])
;;    (let ([graph-lines (read-graph input-port)])
;;      (check-equal?
;;       (part2-generated-path-count graph-lines)
;;       5))))

;; how many common ancestors of out, dac, and fft are there?
(time
 (let ([input-port
        (open-input-file "test-data/input-day11-1.txt")])

   (let ([graph-lines (read-graph input-port)])
     (let ([linked-hash (linked-nodes-hash graph-lines)])
       (let ([linked-from-hash
              (predecessor-nodes-hash linked-hash)])
         (let ([pred-out (find-all-predecessors-of-node-named linked-from-hash "out")]
               [pred-dac (find-all-predecessors-of-node-named linked-from-hash "dac")]
               [pred-fft (find-all-predecessors-of-node-named linked-from-hash "fft")])
                
           (check-equal?
            (set-count pred-out)
            592)

           (check-equal?
            (set-count pred-dac)
            383)

           (check-equal?
            (set-count pred-fft)
            93)

           (check-equal?
            (set-count
             (set-intersect pred-out pred-dac pred-fft))
            93)

           ))))))

; this still takes way too long
; .....counter: 1745500017; [process time 10805.862s] saw-dac: 95791202; saw-fft: 1745500017; sum: 95791202; next-path: (svr yco fnl sdb mzo mfh irq tev fft ikf uzy yoy anf onh jnl cwr vbf flo cnn khf rmm ocz iup yxz dwm uhx you boh vtg saz ywt igz mej out)
(time
 (let ([input-port
        (open-input-file "test-data/input-day11-1.txt")])
   (let ([graph-lines (read-graph input-port)])
     (check-equal?
;      (new-part2-generated-path-count graph-lines)
      (fast-part2-count graph-lines)
      5))))


#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))


(provide
 (contract-out
  ;part 1
  [manual-lexer (-> port? stream?)]
  [read-manual-line-bits-parsed (-> port? stream?)]
  [toggle-switches (-> string? (listof number?) string?)]
  [paths-from (-> string? list? exact-nonnegative-integer? hash?)]
  [paths-from-with-hash (-> list? exact-nonnegative-integer? hash? hash?)]
  [find-length-of-shortest-path (-> string? list? exact-nonnegative-integer?)]
  [total-button-presses (-> port? exact-nonnegative-integer?)]
  ; part 2 (claude.ai)
  [find-minimum-button-presses (-> list? (listof number?) exact-nonnegative-integer?)]
  [total-button-presses-part2 (-> port? exact-nonnegative-integer?)]

  ))

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define (toggle-switches initial-state-string toggle-positions)
  (define (toggle-one state-char)
    (cond [(equal? #\. state-char)
           #\#]

          [(equal? #\# state-char)
           #\.]

          [else (error (format "toggle-one only knows how to toggle between . and #, not ~a" state-char))]))

  (define (iter result-so-far pos initial-remaining)
    (if (null? initial-remaining)
        result-so-far
        (let ([next-char (car initial-remaining)]
              [still-remaining (cdr initial-remaining)])
          (iter
           (cons
            (if (member pos toggle-positions)
                (toggle-one next-char)
                next-char)
            result-so-far)
           (+ pos 1)
           still-remaining))))
          
  (let ([state-length (string-length initial-state-string)])
    (assert (stream-andmap
             (lambda (num) (<= num state-length))
             toggle-positions)
            (format "ouch: all of these positions: ~a should be less than or equal to ~a (length of ~a), but at least one isn't!"
                    toggle-positions
                    state-length
                    initial-state-string))

    (list->string
     (reverse
      (iter '() 0 (string->list initial-state-string))))
    
    ))

; build a hash of states we've been able to reach from an initial state with at most depth steps
(define (paths-from starting-state button-choices max-depth)
  (paths-from-with-hash button-choices max-depth (make-immutable-hash (list (cons starting-state '())))))
                   
; build a version that can start after already exploring some (hash-til-now)
(define (paths-from-with-hash button-choices max-depth hash-til-now)
  (let ([depth-so-far
         (for/fold ([result 0])
                   ([next-node-path (hash-values hash-til-now)])
           (if (> (length next-node-path) result)
               (length next-node-path)
               result))])
    (for/fold ([result hash-til-now])
              ([depth (in-range depth-so-far max-depth)])
      ; expand from frontier, which means those parts of result at distance depth from the start
      (for*/fold ([new-result result])
                 ([next-move button-choices]
                  [next-state (hash-keys result)]
                  #:when (= depth (length (hash-ref result next-state))))
        (let ([next-candidate-state (toggle-switches next-state next-move)])
          ; have we seen this already?
          (cond [(hash-has-key? result next-candidate-state)
                 (let ([previous-path-length (length (hash-ref result next-candidate-state))]
                       [this-path-length (+ 1 (length (hash-ref result next-state)))])
                   ; is this route shorter? (if yes, record shorter path, else leave alone)
                   (if (< this-path-length previous-path-length)
                       (hash-set new-result next-candidate-state (cons next-move (hash-ref result next-state)))
                       new-result))]

                [else
                 (hash-set new-result next-candidate-state (cons next-move (hash-ref result next-state)))]))))))

    
  

(define (find-length-of-shortest-path state-to-reach button-choices)
  (define (shortest-path nodes-from-start nodes-from-finish nodes-that-link)
    (for/fold ([shortest-so-far +inf.0])
              ([next-node nodes-that-link])
      (let ([length-this-way
             (+ (length (hash-ref nodes-from-start next-node))
                (length (hash-ref nodes-from-finish next-node)))])
        (if (< length-this-way shortest-so-far)
            length-this-way
            shortest-so-far))))
  
    (define (iter nodes-from-start nodes-from-finish current-depth)
      (let ([possible-stepping-stones
             (set-intersect
              (hash-keys nodes-from-start)
              (hash-keys nodes-from-finish))])
        (if (not (set-empty? possible-stepping-stones))
            ; we found at least one path
            (shortest-path nodes-from-start nodes-from-finish possible-stepping-stones)
            ; keep looking
            (let ([nodes-from-start
                   (paths-from-with-hash button-choices (+ 1 current-depth) nodes-from-start)]
                  [new-nodes-from-finish
                   (paths-from-with-hash button-choices (+ 1 current-depth) nodes-from-finish)])
              (iter nodes-from-start new-nodes-from-finish (+ 1 current-depth))))))
  (iter
   (let ([state-length (string-length state-to-reach)])
     (let ([initial-state (make-string state-length #\.)])
       (make-immutable-hash (list (cons initial-state '())))))
   (make-immutable-hash (list (cons state-to-reach '())))
   0))


(define (total-button-presses in-port)
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for/fold ([result 0])
              ([next-parsed-line stream-of-parsed-lines]
               [line-number (in-naturals 1)])
      (let ([light-goal (car next-parsed-line)]
            [button-choices (cadr next-parsed-line)])
        ;(printf "line ~a light-goal: ~a; button-choices: ~a~n" line-number light-goal button-choices)
       ; (time
         (let ([sub-total
                (find-length-of-shortest-path light-goal button-choices)])
           ;(printf "line ~a subtotal: ~a~n" line-number sub-total)
           (+ result sub-total))))))
           

; claude's suggestion when I asked for help using a lexter and a parser

;; Token definitions
(define-tokens value-tokens (NUMBER PATTERN))
(define-empty-tokens op-tokens
  (LBRACKET RBRACKET
            LPAREN RPAREN
            LBRACE RBRACE
            COMMA
            DOT HASH
            EOF))

;; Lexer
(define manual-lexer
  (lexer
   ;; Whitespace
   [(:or #\space #\tab #\newline) (manual-lexer input-port)]
   
   ;; Brackets and parens
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   
   ;; Comma
   ["," (token-COMMA)]
   
   ;; Pattern characters (inside brackets)
   ["." (token-DOT)]
   ["#" (token-HASH)]
   
   ;; Numbers
   [(:+ (:or (:/ #\0 #\9))) (token-NUMBER (string->number lexeme))]
   
   ;; EOF
   [(eof) (token-EOF)]))

;; Parser
(define manual-parser
  (parser
   (start line)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (tok-ok? tok-name tok-value)
            (error 'parse "unexpected token: ~a" tok-name)))
   
   (grammar
    
    ;; A complete line
    (line [(pattern tuple-list number-set)
           (list $1 $2 $3)])
    
    ;; Pattern: [.##.]
    (pattern [(LBRACKET pattern-chars RBRACKET)
              (list->string $2)])
    
    (pattern-chars [() '()]
                   [(DOT pattern-chars) (cons #\. $2)]
                   [(HASH pattern-chars) (cons #\# $2)])
    
    ;; Tuple list: (3) (1,3) (2) ...
    (tuple-list [() '()]
                [(tuple tuple-list) (cons $1 $2)])
    
    (tuple [(LPAREN number-list RPAREN) $2])
    
    (number-list [(NUMBER) (list $1)]
                 [(NUMBER COMMA number-list) (cons $1 $3)])
    
    ;; Number set: {3,5,4,7}
    (number-set [(LBRACE number-list RBRACE) $2]))))

;; Helper to parse a single line from a string
(define (parse-manual-line line-str)
  (let ([in (open-input-string line-str)])
    (manual-parser (lambda () (manual-lexer in)))))

;; Read and parse lines from a port
(define (read-manual-line-bits-parsed in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (stream-cons 
         (parse-manual-line next-line)
         (read-manual-line-bits-parsed in-port)))))



;; part 2: I asked claude.ai to solve it (it seemed very busy and I am low on patience today)

;; claude suggested 2 versions of the first function but had reservations; the first failed with an error, the second didn't finish on small, sample input;
;; claude then proposed a 3rd (also with reservations, I think).

;; ;; Solve using integer linear programming approach
;; ;; This is actually a system of linear equations where we want to minimize
;; ;; the sum of button presses subject to constraints
;; (define (find-minimum-button-presses button-choices targets)
;;   (define num-buttons (length button-choices))
;;   (define num-counters (length targets))
;;   
;;   ;; Create a matrix where entry [i][j] = 1 if button j affects counter i
;;   (define (makes-matrix)
;;     (for/list ([counter-idx (in-range num-counters)])
;;       (for/list ([button (in-list button-choices)])
;;         (if (member counter-idx button) 1 0))))
;;   
;;   ;; Brute force with optimization: try different combinations
;;   ;; For small inputs, we can use a greedy approach with bounds
;;   (define (solve-greedy)
;;     (define max-target (apply max targets))
;;     (define upper-bound (* num-buttons max-target)) ;; rough upper bound
;;     
;;     ;; Try to find solution using constraint propagation
;;     (define (try-combination presses-per-button)
;;       (define results
;;         (for/list ([counter-idx (in-range num-counters)])
;;           (for/sum ([button-idx (in-range num-buttons)]
;;                     [button (in-list button-choices)])
;;             (if (member counter-idx button)
;;                 (list-ref presses-per-button button-idx)
;;                 0))))
;;       (and (equal? results targets)
;;            (apply + presses-per-button)))
;;     
;;     ;; Use a smarter search: bounded depth-first search
;;     (define (search-limited max-presses)
;;       (define (search-rec button-idx current-presses current-counters)
;;         (cond
;;           [(= button-idx num-buttons)
;;            (and (equal? current-counters targets)
;;                 (apply + current-presses))]
;;           [else
;;            (for/first ([num-presses (in-range (+ max-presses 1))]
;;                        #:when (let ([new-counters
;;                                       (for/list ([c current-counters]
;;                                                 [i (in-naturals)])
;;                                         (+ c (if (member i (list-ref button-choices button-idx))
;;                                                  num-presses
;;                                                  0)))])
;;                                 (and (for/and ([nc new-counters]
;;                                               [t targets])
;;                                        (<= nc t))
;;                                      (search-rec (+ button-idx 1)
;;                                                (append current-presses (list num-presses))
;;                                                new-counters))))
;;              #f)]))
;;       
;;       (search-rec 0 '() (make-list num-counters 0)))
;;     
;;     ;; Try progressively larger bounds
;;     (for/first ([limit (in-range 0 (+ upper-bound 1))]
;;                 #:when (search-limited limit))
;;       limit))
;;   
;;   (solve-greedy))

;; ;; More efficient: Use mathematical insight
;; ;; This is solving: A * x = b where x >= 0, minimize sum(x)
;; ;; A = button matrix, x = presses per button, b = targets
;; (define (find-minimum-button-presses button-choices targets)
;;   ;; For the contest, you'd typically use Gaussian elimination
;;   ;; with backtracking or a proper ILP solver
;;   ;; Here's a simpler greedy heuristic that works for many cases:
;;   
;;   (define num-buttons (length button-choices))
;;   (define presses (make-vector num-buttons 0))
;;   (define counters (make-vector (length targets) 0))
;;   
;;   ;; Greedy: repeatedly press the button that makes best progress
;;   (let loop ()
;;     (if (equal? (vector->list counters) targets)
;;         (apply + (vector->list presses))
;;         (let* ([best-button
;;                  (argmax
;;                   (lambda (btn-idx)
;;                     (define btn (list-ref button-choices btn-idx))
;;                     ;; Score: how much does this help vs overshoot?
;;                     (for/sum ([c-idx btn])
;;                       (max 0 (- (list-ref targets c-idx)
;;                                (vector-ref counters c-idx)))))
;;                   (range num-buttons))])
;;           (when (> (list-ref (list-ref button-choices best-button) 0) 0) ;; prevent infinite loop
;;             (vector-set! presses best-button (+ 1 (vector-ref presses best-button)))
;;             (for ([c-idx (list-ref button-choices best-button)])
;;               (vector-set! counters c-idx (+ 1 (vector-ref counters c-idx)))))
;;           (loop)))))

; claude's third attempt :)

(define (find-minimum-button-presses button-choices targets)
  (define num-buttons (length button-choices))
  (define target-vector (list->vector targets))
  
  ;; BFS to find minimum total presses
  ;; State: vector of current counter values
  ;; We search for minimum presses to reach target-vector
  (define (state->key state)
    (vector->immutable-vector state))
  
  (define visited (make-hash))
  (define queue (make-queue))
  
  (define start-state (make-vector (length targets) 0))
  (hash-set! visited (state->key start-state) 0)
  (enqueue! queue (cons start-state 0))
  
  (let loop ()
    (if (queue-empty? queue)
        +inf.0  ;; no solution found
        (let* ([item (dequeue! queue)]
               [current-state (car item)]
               [presses-so-far (cdr item)])
          (if (equal? current-state target-vector)
              presses-so-far
              (begin
                ;; Try pressing each button
                (for ([button-idx (in-range num-buttons)])
                  (define button (list-ref button-choices button-idx))
                  (define new-state (vector-copy current-state))
                  
                  ;; Apply button press
                  (for ([counter-idx button])
                    (vector-set! new-state counter-idx 
                                (+ 1 (vector-ref new-state counter-idx))))
                  
                  ;; Only explore if we haven't exceeded targets
                  (when (for/and ([val (in-vector new-state)]
                                  [target targets])
                          (<= val target))
                    (define key (state->key new-state))
                    (define new-presses (+ presses-so-far 1))
                    
                    (unless (and (hash-has-key? visited key)
                                (<= (hash-ref visited key) new-presses))
                      (hash-set! visited key new-presses)
                      (enqueue! queue (cons new-state new-presses)))))
                
                (loop)))))))

;; Part 2 version
(define (total-button-presses-part2 in-port)
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for/fold ([result 0])
              ([next-parsed-line stream-of-parsed-lines]
               [line-number (in-naturals 1)])
      (let ([_light-goal (car next-parsed-line)]  ;; ignore for part 2
            [button-choices (cadr next-parsed-line)]
            [joltage-targets (caddr next-parsed-line)])
        (let ([sub-total
                (find-minimum-button-presses button-choices joltage-targets)])
          (printf "line ~a subtotal: ~a~n" line-number sub-total)
          (+ result sub-total))))))
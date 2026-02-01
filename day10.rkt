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
  [toggle-paths-from (-> string? list? exact-nonnegative-integer? hash?)]
  [toggle-paths-from-with-hash (-> list? exact-nonnegative-integer? hash? hash?)]
  [find-length-of-shortest-toggle-path (-> string? list? exact-nonnegative-integer?)]
  [total-part1-button-presses (-> port? exact-nonnegative-integer?)]
  ; part 2 (my first attempt after several failed attempts by claude.ai)
  [distance-to-goal (-> (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [not-closer-to-goal? (-> (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) boolean?)]
  [part2-paths-from-with-hash (->
                               (listof exact-nonnegative-integer?)
                               (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
                               (listof exact-nonnegative-integer?) exact-nonnegative-integer? hash? hash?)]
  [find-length-of-shortest-part2-path (-> (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) exact-nonnegative-integer?)]
  [find-total-part2-button-presses (-> port? exact-nonnegative-integer?)]

  ; provisionally rejecting linear equations; try greedily getting as close to, but under the goal as we can; how long does that take?
  [joltage-<=? (-> (listof exact-nonnegative-integer?) (listof exact-nonnegative-integer?) boolean?)]
  [part2-greedily-get-close-but-below-goal (-> (listof exact-nonnegative-integer?) (listof (listof exact-nonnegative-integer?)) list?)]
  [greedily-find-length-of-shortest-part2-path (-> string? list? exact-nonnegative-integer?)]
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
(define (toggle-paths-from starting-state button-choices max-depth)
  (toggle-paths-from-with-hash button-choices max-depth (make-immutable-hash (list (cons starting-state '())))))
                   
; build a version that can start after already exploring some (hash-til-now)
(define (toggle-paths-from-with-hash button-choices max-depth hash-til-now)
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

    
  

(define (find-length-of-shortest-toggle-path state-to-reach button-choices)
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
                 (toggle-paths-from-with-hash button-choices (+ 1 current-depth) nodes-from-start)]
                [new-nodes-from-finish
                 (toggle-paths-from-with-hash button-choices (+ 1 current-depth) nodes-from-finish)])
            (iter nodes-from-start new-nodes-from-finish (+ 1 current-depth))))))
  (iter
   (let ([state-length (string-length state-to-reach)])
     (let ([initial-state (make-string state-length #\.)])
       (make-immutable-hash (list (cons initial-state '())))))
   (make-immutable-hash (list (cons state-to-reach '())))
   0))


(define (total-part1-button-presses in-port)
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
               (find-length-of-shortest-toggle-path light-goal button-choices)])
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



;; part 2; I played around asking claude.ai to solve part 2; it was very willing to try, but failed in various ways.
;; thinking about it, it feels as though I can adapt what I did in part 1 (working from start and back from finish until I find a meeting point) -- different, but analogous
;; the first naive approach is too slow however (solves the small sample, but not even line 1 of the real input); too many options and steps, me thinks;
;; what about greedy? what about ruling out steps that take us away from goal (at least until we're close?) ?
;; NB: I explored the idea of using linear algebra, but just the first 10 real cases make this seem infeasible:
;; '("5 equations, 6 unknowns"
;;   "4 equations, 4 unknowns"
;;   "9 equations, 9 unknowns"
;;   "9 equations, 10 unknowns"
;;   "7 equations, 8 unknowns"
;;   "13 equations, 10 unknowns"
;;   "4 equations, 5 unknowns"
;;   "8 equations, 9 unknowns"
;;   "8 equations, 7 unknowns"
;;   "10 equations, 10 unknowns")
;;
;; so now I'm thinking maybe a "greedy" approach might be better:
;; at each step choose whatever button take you closest to the goal (what about ties? flip a coin?) until no button works (any button would take us over the goal);
;; this step would seem to go fast -- o(n) or something
;; not quite sure what to do after that; do we remember where we've been, back off the last step and try an approach like the first one we tried from there (starting much closer
;; to the goal?); if that doesn't work back off again, ...?


(define (distance-to-goal from to)
  (for/fold ([sum 0])
            ([from_i from]
             [to_i to])
    (+ sum
       (abs (- to_i from_i)))))

(define (not-closer-to-goal? at-least-state inquire-state goal-state)
  (let ([at-least-distance (distance-to-goal at-least-state goal-state)]
        [compare-distance  (distance-to-goal inquire-state goal-state)])
    (>= compare-distance at-least-distance)))

(define (joltage-<=? j1 j2)
  (stream-andmap
   (lambda (j-pair) (<= (car j-pair) (cdr j-pair)))
   (apply map cons (list j1 j2))))
                    
(define (apply-op op state move)
  (let ([state-length (length state)])
    (let ([new-state
           (for/list
               ([state_i state]
                [i (in-range state-length)])
             (if (member i move)
                 (op state_i 1)
                 state_i))])
      ;(printf "(apply-op ~a ~a ~a) --> ~a~n" op state move new-state)
      new-state)))                   
  
(define (part2-paths-from-with-hash goal-state add-or-subtract button-choices max-depth hash-til-now)
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
                  [examine-state (hash-keys result)]
                  #:when (= depth (length (hash-ref result examine-state))))
        (let ([next-candidate-state (apply-op add-or-subtract examine-state next-move)])
          ; have we seen this already?
          (cond [(hash-has-key? result next-candidate-state)
                 (let ([previous-path-length (length (hash-ref result next-candidate-state))]
                       [this-path-length (+ 1 (length (hash-ref result examine-state)))])
                   ; is this route shorter? (if yes, record shorter path, else leave alone)
                   (if (< this-path-length previous-path-length)
                       (hash-set new-result next-candidate-state (cons next-move (hash-ref result examine-state)))
                       new-result))]

                [(not-closer-to-goal? examine-state next-candidate-state goal-state)
                 (begin
                   ;(printf " skipping state ~a -- not closer to ~a than ~a is already~n" next-candidate-state goal-state examine-state)
                   new-result)
                 ]
                
                [else
                 (hash-set new-result next-candidate-state (cons next-move (hash-ref result examine-state)))]))))))

(define (find-length-of-shortest-part2-path state-to-reach button-choices)
  (let ([state-length (length state-to-reach)])
    (let ([initial-state (make-list state-length 0)])
      (let ([crude-distance-estimate
             (distance-to-goal initial-state state-to-reach)])
      
        (define (shortest-path nodes-from-start nodes-from-finish nodes-that-link)
          (let-values ([(distance path)
                        (for/fold ([shortest-distance-so-far +inf.0]
                                   [shortest-path-so-far #f])
                                  ([next-node nodes-that-link])
                          (let ([length-this-way
                                 (+ (length (hash-ref nodes-from-start next-node))
                                    (length (hash-ref nodes-from-finish next-node)))])
                            (if (< length-this-way shortest-distance-so-far)
                                (values
                                 length-this-way
                                 (append
                                  (hash-ref nodes-from-start next-node)
                                  (hash-ref nodes-from-finish next-node)))
                                (values shortest-distance-so-far shortest-path-so-far))))])
            (printf "shortest-path: ~a~n" path)
            distance))
  
        (define (iter nodes-from-start nodes-from-finish current-depth)
          (cond
            [(> current-depth crude-distance-estimate)
             (error (format "  are we sure we should ever need more steps than crude-distance-estimate (~a)? (our current-depth is ~a)" crude-distance-estimate current-depth))]

            [else
             (begin
               (let ([possible-stepping-stones
                      (set-intersect
                       (hash-keys nodes-from-start)
                       (hash-keys nodes-from-finish))])
                 (if (not (set-empty? possible-stepping-stones))
                     ; we found at least one path
                     (shortest-path nodes-from-start nodes-from-finish possible-stepping-stones)
                     ; keep looking
                     (let ([nodes-from-start
                            (part2-paths-from-with-hash state-to-reach + button-choices (+ 1 current-depth) nodes-from-start)]
                           [new-nodes-from-finish
                            (part2-paths-from-with-hash state-to-reach - button-choices (+ 1 current-depth) nodes-from-finish)])
                       (iter nodes-from-start new-nodes-from-finish (+ 1 current-depth))))))]))


        (begin
          (printf "--- (find-length-of-shortest-part2-path ~a ~a)~n" state-to-reach button-choices)
          (printf "--- NB: crude-distance-estimate: ~a~n" crude-distance-estimate)
        
          (let ([result
                 (iter
                  (make-immutable-hash (list (cons initial-state '())))
                  (make-immutable-hash (list (cons state-to-reach '())))
                  0)])
            (printf "--- ---> ~a~n" result)
            result))))))
    

(define (find-total-part2-button-presses in-port)
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for/fold ([result 0])
              ([next-parsed-line stream-of-parsed-lines]
               [line-number (in-naturals 1)])
      (let ([joltage-goal (caddr next-parsed-line)]
            [button-choices (cadr next-parsed-line)])
        (printf "line ~a joltage-goal: ~a; button-choices: ~a~n" line-number joltage-goal button-choices)
        ;(time
        (let ([sub-total
               (find-length-of-shortest-part2-path joltage-goal button-choices)])
          (printf "line ~a subtotal: ~a~n" line-number sub-total)
          (+ result sub-total))))))


(define (part2-greedily-get-close-but-below-goal state-to-reach button-choices)
  (define (not-too-far? button state-so-far)
    (let ([state-if (apply-op + state-so-far button)])
      (joltage-<=? state-if state-to-reach)))

  (define (button-size button)
    (for/fold ([sum 0])
              ([sub-button button])
      (+ sum sub-button)))

  (define (best-button current-state buttons)
    (let-values ([(the-best-button best-distance)
                  (for/fold ([best-button-so-far (car buttons)]
                             [best-distance-so-far
                              (distance-to-goal
                               (apply-op + current-state (car buttons))
                               state-to-reach)])
                            ([next-button (cdr buttons)])
                    (let ([distance-from-next-button
                           (distance-to-goal
                            (apply-op + current-state next-button)
                            state-to-reach)])
                      (if (< distance-from-next-button best-distance-so-far)
                          (values next-button distance-from-next-button)
                          (values best-button-so-far best-distance-so-far))))])
      the-best-button))
  
  (define (iter path-so-far state-so-far)
    (let ([next-options
           (filter (lambda (button) (not-too-far? button state-so-far))
                   button-choices)])
      (if (null? next-options)
          path-so-far
          (let ([greedy-button
                 (best-button state-so-far next-options)])
            (iter
             (cons greedy-button path-so-far)
             (apply-op + state-so-far greedy-button))))))

  (printf "(part2-greedily-get-close-but-below-goal ~a ~a)~n" state-to-reach button-choices)
  (let ([state-length (length state-to-reach)])
    (let ([initial-state (make-list state-length 0)])
      (let ([result
             (iter '() initial-state)])
        (printf "--> ~a~n" result)
        (let ([would-take-us-to
               (for/fold ([state initial-state])
                         ([button result])
                 (apply-op + state button))])
          (printf " (this would take us to ~a (~a short of goal: ~a)~n"
                  would-take-us-to
                  (distance-to-goal
                   would-take-us-to
                   state-to-reach)
                  state-to-reach)
          result
          )))))

(define (greedily-find-length-of-shortest-part2-path state-to-reach button-choices)
  -1)
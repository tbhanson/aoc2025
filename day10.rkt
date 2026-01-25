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
    [find-minimum-button-presses (-> (listof (listof exact-nonnegative-integer?))
                                    (listof exact-nonnegative-integer?)
                                    exact-nonnegative-integer?)]
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
;; I've tried a series of its attempts! :)

;; Add these to your existing day10.rkt file

;; Part 2: Find minimum button presses to reach target counter values
;; Using a more robust search with better heuristics


(define (find-minimum-button-presses button-choices targets)
  ;; Use A* search with Manhattan distance heuristic
  ;; This is much more efficient than plain BFS
  
  (define num-counters (length targets))
  
  (define (state->key state)
    (list->vector state))
  
  (define (press-button counters button)
    (for/list ([counter-val counters]
               [counter-idx (in-naturals)])
      (if (member counter-idx button)
          (+ counter-val 1)
          counter-val)))
  
  (define (exceeds-target? counters)
    (for/or ([val counters]
             [target targets])
      (> val target)))
  
  ;; Heuristic: minimum additional presses needed (optimistic estimate)
  (define (heuristic state)
    (define remaining
      (for/list ([val state]
                 [target targets])
        (max 0 (- target val))))
    (apply max remaining))  ;; At minimum, need max of remaining
  
  ;; Priority queue using hash with priorities
  (define pq (make-hash))
  (define (pq-insert! state priority)
    (hash-set! pq state (min priority (hash-ref pq state +inf.0))))
  
  (define (pq-pop-min!)
    (define min-state
      (for/fold ([best-state #f]
                 [best-priority +inf.0]
                 #:result best-state)
                ([(state priority) (in-hash pq)])
        (if (< priority best-priority)
            (values state priority)
            (values best-state best-priority))))
    (hash-remove! pq min-state)
    min-state)
  
  ;; A* search
  (define dist (make-hash))
  (define initial-state (make-list num-counters 0))
  (define initial-key (state->key initial-state))
  
  (hash-set! dist initial-key 0)
  (pq-insert! initial-state (heuristic initial-state))
  
  (let loop ()
    (cond
      [(hash-empty? pq) +inf.0]
      
      [else
       (define current-state (pq-pop-min!))
       (define current-key (state->key current-state))
       (define current-dist (hash-ref dist current-key +inf.0))
       
       (cond
         [(equal? current-state targets) current-dist]
         
         [else
          ;; Try each button
          (for ([button button-choices])
            (define new-state (press-button current-state button))
            
            (unless (exceeds-target? new-state)
              (define new-key (state->key new-state))
              (define new-dist (+ current-dist 1))
              (define old-dist (hash-ref dist new-key +inf.0))
              
              (when (< new-dist old-dist)
                (hash-set! dist new-key new-dist)
                (pq-insert! new-state (+ new-dist (heuristic new-state))))))
          
          (loop)])])))

(define (total-button-presses-part2 in-port)
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for/fold ([result 0])
              ([next-parsed-line stream-of-parsed-lines]
               [line-number (in-naturals 1)])
      (let ([_light-goal (car next-parsed-line)]
            [button-choices (cadr next-parsed-line)]
            [joltage-targets (caddr next-parsed-line)])
        (printf "Processing line ~a with targets ~a...~n" line-number joltage-targets)
        (let ([sub-total
               (time (find-minimum-button-presses button-choices joltage-targets))])
          (printf "line ~a subtotal: ~a~n" line-number sub-total)
          (+ result sub-total))))))
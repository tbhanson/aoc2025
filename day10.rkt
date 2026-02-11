#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

; coming back to part 2 of this after day 11: part 1 solved by me, part 2 solved by claude (elegant "dynamic programming" solution")
; recall that I let Claude have several passes at part 2 of this one, each attempt failed.
; my brain has tried out ideas in the meantime and I think I want to pursue this one:
; (vaguely divide and conquer, perhaps using some ideas from previous attempts with linear algebra and greedy)
; more precisely:
; we convert the buttons to binary vectors V_i
; (as we did for linear algebra attempts, though we couldn't see an LA solution, since various problems (lines) yieled number of equations less, equal, and sometimes more than number of unknowns)
; now we proceed one "joltage level counter" at a time
; e.g. for [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
; for each C_j of {3,5,4,7} we enumerate all the ways our V_i can be combined to yield C_j (call these K_ij ?)
; then we "combine" by finding smallest combination (smallest sum of coefficients) that solves all C_j

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
  [find-minimum-button-presses (-> (listof (listof exact-nonnegative-integer?))
                                   (listof exact-nonnegative-integer?)
                                   exact-nonnegative-integer?)]
  [total-button-presses-part2 (-> port? number?)]
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


; I decided to ask Claude to implement my idea
;; Minimum button presses for joltage configuration - Claude
;; https://claude.ai/chat/26639470-8d40-4003-a031-7d475686f961

;; Add these to your existing day10.rkt file

;; Part 2: Greedy solve per counter + iterative relaxation to find common solution


(define (find-minimum-button-presses button-choices targets)
  (define num-buttons (length button-choices))
  (define num-counters (length targets))
  
  ;; Calculate current counter values from button presses
  (define (calculate-counters presses)
    (for/vector ([counter-idx (in-range num-counters)])
      (for/sum ([button-idx (in-range num-buttons)]
                #:when (member counter-idx (list-ref button-choices button-idx)))
        (vector-ref presses button-idx))))
  
  ;; Greedy initial solution: for each counter, distribute target among affecting buttons
  ;; Prefer buttons that affect fewer counters (more specific)
  (define initial-presses (make-vector num-buttons 0))
  
  (for ([counter-idx (in-range num-counters)])
    (define target (list-ref targets counter-idx))
    (define affecting-buttons
      (sort
       (for/list ([button-idx (in-range num-buttons)]
                  #:when (member counter-idx (list-ref button-choices button-idx)))
         button-idx)
       <
       #:key (lambda (btn-idx) (length (list-ref button-choices btn-idx)))))
    
    (unless (null? affecting-buttons)
      ;; Use the most specific button (affects fewest counters) to reach target
      (define best-button (car affecting-buttons))
      (vector-set! initial-presses best-button 
                  (max (vector-ref initial-presses best-button) target))))
  
  ;; Now iteratively adjust to satisfy all counters
  (define presses (vector-copy initial-presses))
  
  (let loop ([iterations 0])
    (define current-counters (calculate-counters presses))
    
    ;; Check if all counters satisfied
    (define all-satisfied?
      (for/and ([current (in-vector current-counters)]
                [target targets])
        (= current target)))
    
    (cond
      [all-satisfied?
       (apply + (vector->list presses))]
      
      [(> iterations 100000)
       (error "Failed to converge")]
      
      [else
       ;; Find counter with largest error
       (define-values (worst-counter error-amount)
         (for/fold ([worst-idx 0]
                    [worst-error 0])
                   ([counter-idx (in-range num-counters)])
           (define error (- (list-ref targets counter-idx) 
                           (vector-ref current-counters counter-idx)))
           (if (> (abs error) (abs worst-error))
               (values counter-idx error)
               (values worst-idx worst-error))))
       
       ;; Adjust buttons that affect this counter
       (define affecting-buttons
         (for/list ([button-idx (in-range num-buttons)]
                    #:when (member worst-counter (list-ref button-choices button-idx)))
           button-idx))
       
       (cond
         [(null? affecting-buttons)
          (error "No buttons affect counter ~a" worst-counter)]
         
         [(> error-amount 0)
          ;; Need more - press the most specific button once
          (define best-button
            (argmin (lambda (btn) (length (list-ref button-choices btn)))
                    affecting-buttons))
          (vector-set! presses best-button (+ 1 (vector-ref presses best-button)))
          (loop (+ iterations 1))]
         
         [else
          ;; Need less - try to decrease a button
          ;; Find button we can decrease without breaking other counters
          (define button-to-decrease
            (for/first ([button-idx affecting-buttons]
                        #:when (> (vector-ref presses button-idx) 0))
              button-idx))
          
          (if button-to-decrease
              (begin
                (vector-set! presses button-to-decrease 
                            (- (vector-ref presses button-to-decrease) 1))
                (loop (+ iterations 1)))
              ;; Can't decrease any button - try increasing another counter to compensate
              ;; This is a conflict - try different approach
              (let ([other-affecting-buttons
                     (filter (lambda (btn) 
                              (for/or ([other-counter (list-ref button-choices btn)])
                                (and (not (= other-counter worst-counter))
                                     (< (vector-ref current-counters other-counter)
                                        (list-ref targets other-counter)))))
                             affecting-buttons)])
                (if (null? other-affecting-buttons)
                    (error "Cannot resolve conflict at counter ~a" worst-counter)
                    (begin
                      ;(define btn (car other-affecting-buttons))
                      (vector-set! presses (car other-affecting-buttons) (+ 1 (vector-ref presses (car other-affecting-buttons))))
                      (loop (+ iterations 1))))))])])))

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
               (with-handlers ([exn:fail? (lambda (e) 
                                           (printf "Error on line ~a: ~a~n" line-number (exn-message e))
                                           0)])
                 (time (find-minimum-button-presses button-choices joltage-targets)))])
          (printf "line ~a subtotal: ~a~n" line-number sub-total)
          (+ result sub-total))))))
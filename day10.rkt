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
  [fewest-presses (-> string? list? exact-nonnegative-integer?)]
  [paths-from (-> string? list? exact-nonnegative-integer? hash?)]
  [paths-from-with-hash (-> list? exact-nonnegative-integer? hash? hash?)]
  [find-shortest-path (-> string? list? exact-nonnegative-integer?)]
  [total-button-presses (-> port? exact-nonnegative-integer?)]
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

    
  

(define (find-shortest-path state-to-reach button-choices)
  ;;   (define (iter nodes-from-start nodes-from-finish current-depth)
  ;;     (let ([possible-stepping-stones
  ;;            (set-intersect
  ;;             (hash-keys nodes-from-start)
  ;;             (hash-keys nodes-from-finish))])
  ;;       (if (not (set-empty? possible-stepping-stones))
  ;;           ; we found a shortest path
  ;;           (let ([node-on-path (set-first possible-stepping-stones)])
  ;;             (+ (length (hash-ref nodes-from-start node-on-path))
  ;;                (length (hash-ref nodes-from-finish node-on-path))))
  ;;           ; keep looking
  ;;           (let ([new-nodes-from-start
                 
  '())

(define (fewest-presses state-to-reach button-choices)
  (define (breadth-first state-to-here remaining-button-choices buttons-tried-so-far)
    ;(printf "(breadth-first ~a ~a ~a)~n" state-to-here remaining-button-choices buttons-tried-so-far)
    (cond [(string=? state-to-here state-to-reach)
           (let ([result
                  (length buttons-tried-so-far)])
             ;(printf " result: ~a~n" result)
             result)]

          [(null? remaining-button-choices)
           +inf.0] ; we have nothing left to try on this branch

          [else
           (for/fold ([result +inf.0])
                     ([next-button remaining-button-choices])
             (let ([suppose-next-button
                    (breadth-first
                     (toggle-switches state-to-here next-button)
                     (remove next-button remaining-button-choices)
                     (cons next-button buttons-tried-so-far))])
               (if (< suppose-next-button result)
                   suppose-next-button
                   result)))]))

  (let ([state-length (string-length state-to-reach)])
    (let ([initial-state (make-string state-length #\.)])
        
      (breadth-first initial-state button-choices '()))))


(define (total-button-presses in-port)
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for/fold ([result 0])
              ([next-parsed-line stream-of-parsed-lines]
               [line-number (in-naturals 1)])
      (let ([light-goal (car next-parsed-line)]
            [button-choices (cadr next-parsed-line)])
        (printf "line ~a light-goal: ~a; button-choices: ~a~n" line-number light-goal button-choices)
        (time
         (let ([sub-total
                (fewest-presses light-goal button-choices)])
           (printf "line ~a subtotal: ~a~n" line-number sub-total)
           (+ result sub-total)))))))
           

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


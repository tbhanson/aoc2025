#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-forklift-grid (-> port? vector?)]
;;    [max-joltage (-> string? exact-nonnegative-integer?)]
;;    [read-joltage-strings (-> port? stream?)]
;;    [total-joltage (-> stream? exact-nonnegative-integer?)]

  )
 )


(define forklift-grid-lexer
  (lexer
   [(concatenation (repetition 1 999 (char-set ".@")))
   
    (begin
      ;(printf "saw joltage string: ~a~n" lexeme)
      (stream-cons
       (list->vector
        (string->list lexeme))
       (forklift-grid-lexer input-port)))]
      
   [whitespace
    (forklift-grid-lexer input-port)]

   [(eof)
    empty-stream]
   ))

(define (read-forklift-grid a-port)
  (list->vector
   (stream->list
    (forklift-grid-lexer a-port))))
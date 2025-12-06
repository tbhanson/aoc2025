#lang racket

(require parser-tools/lex)


(provide
 (contract-out
   [max-joltage (-> string? exact-nonnegative-integer?)]
   [read-joltage-strings (-> port? stream?)]
   [total-joltage (-> stream? exact-nonnegative-integer?)]
  )
 )

(define (max-joltage batteries-string)
  (for/fold ([result 0])
            ([idx1 (in-range (string-length batteries-string))])
    ;(printf " idx1: ~a; ch@idx1: ~a~n" idx1 (string-ref batteries-string idx1))
    (max
     result
     (for/fold ([sub-result 0])
               ([idx2 (in-range (+ idx1 1) (string-length batteries-string))])
       ;(printf " idx2: ~a; ch@idx2: ~a~n" idx2 (string-ref batteries-string idx2))
       (let ([next-try
              (string->number
               (list->string
                (list
                 (string-ref batteries-string idx1)
                 (string-ref batteries-string idx2))))])
         ;(printf " next-try: ~a~n" next-try)
     
         (max
          sub-result
          next-try))))))

(define joltage-string-lexer
  (lexer
   [(concatenation (repetition 1 999 (char-set "0123456789")))
   
    (begin
      ;(printf "saw joltage string: ~a~n" lexeme)
      (stream-cons
       lexeme
       (joltage-string-lexer input-port)))]
      
   [whitespace
    (joltage-string-lexer input-port)]

   [(eof)
    empty-stream]
   ))

(define (read-joltage-strings a-port)
   (joltage-string-lexer a-port))

(define (total-joltage stream-of-joltage-strings)
  ;(printf "(total-joltage stream-of-joltage-strings)~n")
  (for/fold ([sum 0])
            ([joltage-string stream-of-joltage-strings])
    (let ([next-max-joltage (max-joltage joltage-string)])
      ;(printf " next-max-joltage: ~a~n" next-max-joltage)
      (+ sum next-max-joltage))))
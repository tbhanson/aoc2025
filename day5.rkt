#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-ID-ranges (-> port? stream?)]
  [read-IDs (-> port? stream?)]
  )
 )

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define ID-range-lexer
  (lexer
   [(concatenation (repetition 1 99 numeric) (char-set "-") (repetition 1 99 numeric))
    
    (let ([split-lexeme (string-split lexeme "-")])
      (stream-cons
       (cons (string->number (car split-lexeme))
             (string->number (cadr split-lexeme)))
 
       (ID-range-lexer input-port)))]

   ; normal end
   [(concatenation #\newline #\newline)
    empty-stream]

   ; safety end
   [(eof)
    empty-stream]

   [#\newline
    (ID-range-lexer input-port)]
   ))

(define (read-ID-ranges a-port)
    (ID-range-lexer a-port))


(define ID-lexer
  (lexer
   [(repetition 1 99 numeric)
    
    (stream-cons
     (string->number lexeme)
     
     (ID-lexer input-port))]

   [(eof)
    empty-stream]

   [#\newline
    (ID-lexer input-port)]
   ))

(define (read-IDs a-port)
    (ID-lexer a-port))


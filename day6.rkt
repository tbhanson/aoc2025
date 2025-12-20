#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-cephalapod-arithmetic (-> port? stream?)]
  [solve-cephalapod-arithmetic (-> stream? exact-nonnegative-integer?)]
  )
 )

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

;(define-lex-abbrev numb #rx"[0-9]+")
;(define-lex-abbrev op #rx"[+*]")
;(define-lex-abbrev space #rx"[0-9]+")

(define cephalapod-arithmetic-lexer
  
  (lexer
   [(concatenation (repetition 0 1 #\space) (repetition 1 99 numeric)
                   (repetition 0 99 (concatenation (repetition 1 10 #\space) (repetition 1 99 numeric)))
                   #\newline)

    (let ([list-of-numbers (map string->number (string-split lexeme))])
      (stream-cons
       list-of-numbers
       (cephalapod-arithmetic-lexer input-port)))]

   [(concatenation (repetition 0 1 #\space) (char-set "+*")
                   (repetition 0 99 (concatenation (repetition 1 10 #\space) (char-set "+*")))
                   #\newline)
    
    (let ([list-of-ops (string-split lexeme)])
      (stream-cons
       list-of-ops
       (cephalapod-arithmetic-lexer input-port)))]

   [(eof)
    empty-stream]
   
   ))

(define (read-cephalapod-arithmetic a-port)
    (cephalapod-arithmetic-lexer a-port))



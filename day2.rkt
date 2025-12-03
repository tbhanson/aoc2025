#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  ; struct automatics
  [string->ID-range (-> string? (values exact-nonnegative-integer? exact-nonnegative-integer?))]
  [read-ID-ranges (-> port? stream?)]
  )
 )

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define (string->ID-range str)
  (let ([parts (string-split str "-")])
    (begin
      (assert (= (length parts) 2) (format "ID-range should have two integers separated by dash, not ~a" str))
      (let ([lo (string->number (car parts))]
            [hi (string->number (cadr parts))])
        (assert (< lo hi) (format "ID-range should have two integers lo and hi, separated by dash, with lo<hi, but lo = ~a, hi = ~a" lo hi))
        (values lo hi)
        )
      )))
        
      
      
(define ID-range-lexer
  (lexer
   [(concatenation (repetition 1 99 (char-set "0123456789")) #\- (repetition 1 99 (char-set "0123456789")))
   
    (begin
      ;(printf "saw ID-range: ~a~n" lexeme)
      (let-values ([(lo hi) (string->ID-range lexeme)])
        (stream-cons
         (cons lo hi)
         (ID-range-lexer input-port))))]
      
   [(union #\, #\space #\newline #\tab)
    (ID-range-lexer input-port)]

   [(eof)
    empty-stream]
   ))

(define (read-ID-ranges a-port)
   (ID-range-lexer a-port))


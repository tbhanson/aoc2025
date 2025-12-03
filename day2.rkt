#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  ; struct automatics
  [string->ID-range (-> string? (values exact-nonnegative-integer? exact-nonnegative-integer?))]
  [read-ID-ranges (-> port? stream?)]
  [invalid-ID (-> exact-nonnegative-integer? boolean?)]
  [invalid-IDs-in-range (-> exact-nonnegative-integer? exact-nonnegative-integer? stream?)]
  [sum-invalid-IDs-in-ID-range-stream (-> stream? exact-nonnegative-integer?)]
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

(define (invalid-ID id)
  (let ([id-as-str (number->string id)])
    (let ([id-str-len (string-length id-as-str)])
      (and
       (= 
        (remainder id-str-len 2)
        0)
       (string=?
        (substring id-as-str 0 (/ id-str-len 2))
        (substring id-as-str (/ id-str-len 2)))))))
                

(define (invalid-IDs-in-range lo hi)
  (stream-filter
   invalid-ID
   (in-range lo (+ hi 1))
   ))


(define (sum-invalid-IDs-in-ID-range-stream ID-range-stream)
  (for/fold ([result 0])
            ([next-range ID-range-stream])
    (let ([next-partial-sum
           (stream-fold
            + 0
            (invalid-IDs-in-range
             (car next-range)
             (cdr next-range)))])
      (+ result next-partial-sum))))
    
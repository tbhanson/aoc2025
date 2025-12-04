#lang racket

(require parser-tools/lex)
(require math/number-theory)


(provide
 (contract-out
  ; struct automatics
  [string->ID-range (-> string? (values exact-nonnegative-integer? exact-nonnegative-integer?))]
  [read-ID-ranges (-> port? stream?)]
  [invalid-ID (-> exact-nonnegative-integer? boolean?)]
  [invalid-IDs-in-range (-> exact-nonnegative-integer? exact-nonnegative-integer? stream?)]
  [sum-invalid-IDs-in-ID-range-stream (-> stream? exact-nonnegative-integer?)]
  ; part 2
  [string-is-N-copies? (-> string? exact-nonnegative-integer? boolean?)]
  [invalid-ID-2 (-> exact-nonnegative-integer? boolean?)]
  [sum-invalid-IDs-2-in-ID-range-stream (-> stream? exact-nonnegative-integer?)]
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


(define (string-is-N-copies? a-string n)
  ;(printf "(string-is-N-copies? ~a ~a)~n" a-string n)
  (let ([sub-str-len (/ (string-length a-string) n)])
    ;(printf " sub-str-len: ~a~n" sub-str-len)
    
    (let ([prefix (substring a-string 0 sub-str-len)])
      ;(printf " prefix: ~a~n" prefix)
      
      (for/fold ([result #t])
                ([i (in-range n)])
        ;(printf " i: ~a~n" i)
        (let ([next-result
               (and result
                    (string=?
                     prefix
                     (substring
                      a-string
                      (* i sub-str-len)
                      (* (+ i 1) sub-str-len))))])
          ;(printf " next-result: ~a~n" next-result)
          next-result)))))
       
; now we have to consider any number of copies 2 or more
(define (invalid-ID-2 id)
  (let ([id-as-str (number->string id)])
    (let ([id-str-len (string-length id-as-str)])
      (define (iter remaining-divisors)
        (cond [(null? remaining-divisors)
               #f]

              [(string-is-N-copies? id-as-str (car remaining-divisors))
               #t]

              [else
               (iter (cdr remaining-divisors))]))

      (let ([try-divisors
             (cdr (divisors id-str-len))]) ; we need to try all divisors except 1
        (iter try-divisors)))))

(define (invalid-IDs-2-in-range lo hi)
  (stream-filter
   invalid-ID-2
   (in-range lo (+ hi 1))
   ))

(define (sum-invalid-IDs-2-in-ID-range-stream ID-range-stream)
  (for/fold ([result 0])
            ([next-range ID-range-stream])
    (let ([next-partial-sum
           (stream-fold
            + 0
            (invalid-IDs-2-in-range
             (car next-range)
             (cdr next-range)))])
      (+ result next-partial-sum))))

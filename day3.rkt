#lang racket

(require parser-tools/lex)


(provide
 (contract-out
   [max-joltage (-> string? exact-nonnegative-integer?)]
   [read-joltage-strings (-> port? stream?)]
   [total-joltage (-> stream? exact-nonnegative-integer?)]

   [max-joltage-2 (-> string? exact-nonnegative-integer?)]
   [total-joltage-2  (-> stream? exact-nonnegative-integer?)]
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

; now we need to greedily gather 12 digits from left to right!
(define (max-joltage-2 batteries-string)
  ; greedily collect the biggest digit scanning the string-remaining left to right, but not going closer to the end than digits-still-to-collect
  (define (iter result-so-far digits-still-to-collect string-remaining)
;;     (printf "(iter [~a] ~a ~a)~n" result-so-far digits-still-to-collect string-remaining)
    (cond [(<= digits-still-to-collect 0)
           result-so-far]

          [else
           (let ([remaining-string-length (string-length string-remaining)])
;;              (printf " remaining-string-length: ~a~n" remaining-string-length)
             
             (let ([eligible-sub-string
                    (substring string-remaining
                               0
                               (- remaining-string-length (- digits-still-to-collect 1)))])
;;                (printf " eligible-sub-string: ~a~n" eligible-sub-string)
               (let-values ([(next-digit new-string-remaining)
                             (for/fold ([best-digit (string-ref eligible-sub-string 0)]
                                        [string-after-best (substring string-remaining 1)])
                                       ([digit (substring eligible-sub-string 1)]
                                        [pos (in-range 1 (string-length eligible-sub-string))])
;;                                (printf "  best-digit: ~a string-after-best: ~a~n" best-digit string-after-best)
;;                                (printf "  digit: ~a pos: ~a~n" digit pos)
                               (if (char>? digit best-digit)
                                   (values digit (substring string-remaining (+ pos 1))) ; choose a better character
                                   (values best-digit string-after-best)))])
                 (iter
                  (string-append result-so-far (list->string (list next-digit)))
                  (- digits-still-to-collect 1)
                  new-string-remaining))))]))
  
  (string->number
   (iter "" 12 batteries-string)))

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

(define (total-joltage-2 stream-of-joltage-strings)
    (for/fold ([sum 0])
            ([joltage-string stream-of-joltage-strings])
    (let ([next-max-joltage (max-joltage-2 joltage-string)])
      (+ sum next-max-joltage))))

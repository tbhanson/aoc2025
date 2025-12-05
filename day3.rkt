#lang racket

(require parser-tools/lex)


(provide
 (contract-out
   [max-joltage (-> string? exact-nonnegative-integer?)]
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


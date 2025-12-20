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

(define cephalapod-arithmetic-lexer
  
  (lexer
   [(concatenation (repetition 1 9999 (char-set "0123456789 "))
    #\newline)

    (let ([list-of-numbers (map string->number (string-split lexeme))])
      (stream-cons
       list-of-numbers
       (cephalapod-arithmetic-lexer input-port)))]

   [(concatenation (repetition 1 9999 (char-set "+* "))
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

(define (solve-cephalapod-arithmetic line-stream)
  (let ([number-list-stream
         (stream-filter
          (lambda (line) (number? (car line)))
          line-stream)]
        [operator-line
         (stream-ref
          line-stream
          (- (stream-length line-stream) 1))])
    (let ([column-answers
           (for/fold ([answers (stream-first number-list-stream)])
                     ([next-numbers-to-combine (stream-rest number-list-stream)])
;;              (printf "answers: ~a~n" answers)
;;              (printf "next-numbers-to-combine: ~a~n" next-numbers-to-combine)
             (let ([new-answers
                    (for/list
                        ([operand1 answers]
                         [operand2 next-numbers-to-combine]
                         [operator operator-line])
                      (cond [(equal? operator "+")
                             (+ operand1 operand2)]
                            [(equal? operator "*")
                             (* operand1 operand2)]
                            [else (error (format "expected operator but got ~a (to combine ~a with ~a)" operator operand1 operand2))]))])
               new-answers))])
      (for/fold ([sum 0])
                ([column-result column-answers])
        (+ sum column-result)))))
                            
         
#lang racket

(require parser-tools/lex)
(require math/array)


(provide
 (contract-out
  [read-cephalapod-arithmetic (-> port? stream?)]
  [solve-cephalapod-arithmetic (-> stream? exact-nonnegative-integer?)]

  [read-lines (-> port? stream?)] ; stream of lines top to bottom as expected
  [vector-vector-transpose (-> vector? vector?)]
  [read-cephalapod-arithmetic-part-2 (-> port? vector?)]
  [solve-cephalapod-arithmetic-part-2 (-> stream? exact-nonnegative-integer?)]
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


; and now for something completely different

(define (read-lines a-port)
  (let ([next-line (read-line a-port)])
    (if (eof-object? next-line)
        empty-stream
        (stream-cons next-line (read-lines a-port)))))

(define (vector-vector-transpose v-v)
  (let ([row-count (vector-length v-v)]
        [col-count (vector-length (vector-ref v-v 0))])
    (list->vector
     (reverse
      (for/fold ([new-rows-list '()])
                ([new-row-num (in-range col-count)])
        (let ([next-row
               (for/list ([old-row (in-range row-count)])
                 (vector-ref (vector-ref v-v old-row) new-row-num))])
          (cons
           (list->vector next-row)
           new-rows-list)))))))

(define (read-cephalapod-arithmetic-part-2 a-port)
  (let ([vector-of-line-vectors
         (list->vector
          (stream->list
           (stream-map
            (lambda (line)
              (printf "line: ~a~n" line)
              (list->vector
               (string->list line)))
            (read-lines a-port))))])
    (printf "vector-of-line-vectors: ~a~n" vector-of-line-vectors)
    (vector-vector-transpose
     vector-of-line-vectors)))
     
   

(define (solve-cephalapod-arithmetic-part-2 line-stream)
  0)
         
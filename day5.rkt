#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-IDs-and-ranges (-> port? stream?)]
  [count-of-fresh-IDs (-> stream? stream? exact-nonnegative-integer?)]
  [input->fresh-ID-count (-> stream? exact-nonnegative-integer?)]
  
  )
 )

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define ID-and-range-lexer
  (lexer
   [(concatenation (repetition 1 99 numeric) (char-set "-") (repetition 1 99 numeric))
    
    (let ([split-lexeme (string-split lexeme "-")])
      (stream-cons
       (cons (string->number (car split-lexeme))
             (string->number (cadr split-lexeme)))
 
       (ID-and-range-lexer input-port)))]

   [(repetition 1 99 numeric)
    
    (stream-cons
     (string->number lexeme)
     
     (ID-and-range-lexer input-port))]


   [whitespace
    (ID-and-range-lexer input-port)]

   [(eof)
    empty-stream]
   
   ))

(define (read-IDs-and-ranges a-port)
    (ID-and-range-lexer a-port))


(define (count-of-fresh-IDs ID-range-stream ID-stream)
  (for/fold ([sum 0])
            ([next-ID ID-stream])
    (if (stream-empty?
         (stream-filter
          (lambda (next-id-range)
            (let ([lo (car next-id-range)]
                  [hi (cdr next-id-range)])
              (and
               (<= lo next-ID)
               (<= next-ID hi))))
          ID-range-stream))
        sum
        (+ sum 1)
        )))

(define (input->fresh-ID-count mixed-stream)
  (let ([ID-ranges
         (stream-filter pair? mixed-stream)]
        [IDs
         (stream-filter number? mixed-stream)])
    (count-of-fresh-IDs ID-ranges IDs)))

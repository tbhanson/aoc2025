#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-IDs-and-ranges (-> port? stream?)]
  [set-of-fresh-IDs (-> stream? set?)]
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

(define (set-of-fresh-IDs ID-ranges-stream)
  (for/fold ([result (set)])
            ([next-ID-range-pair ID-ranges-stream])
    (let ([next-range
           (in-range
            (car next-ID-range-pair)
            (+ 1 (cdr next-ID-range-pair)))])
      (set-union
       result
       (list->set
        (stream->list
         next-range))))))

(define (count-of-fresh-IDs ID-range-stream ID-stream)
  (let ([fresh-IDs (set-of-fresh-IDs ID-range-stream)])
    (for/fold ([sum 0])
              ([next-ID ID-stream])
      (if (set-member? fresh-IDs next-ID)
          (+ sum 1)
          sum))))

(define (input->fresh-ID-count mixed-stream)
  (let ([ID-ranges
         (stream-filter pair? mixed-stream)]
        [IDs
         (stream-filter number? mixed-stream)])
    (count-of-fresh-IDs ID-ranges IDs)))

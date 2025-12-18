#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-IDs-and-ranges (-> port? stream?)]
  [count-of-fresh-IDs (-> stream? stream? exact-nonnegative-integer?)]
  [input->fresh-ID-count (-> stream? exact-nonnegative-integer?)]
  [gather-and-sort-ranges (-> port? (listof pair?))]
  [combine-ranges (-> (listof pair?) (listof pair?))]
  [fresh-ID-count-by-ranges (-> port? exact-nonnegative-integer?)]
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

(define (sort-ranges ranges)
  (sort 
   ranges
   (lambda (p1 p2)
     (<= (car p1)
         (car p2)))))
   
  
         
(define (gather-and-sort-ranges in-port)
  (let ([mixed-stream
         (read-IDs-and-ranges in-port)])
    (let ([sorted-ID-ranges
           (sort-ranges
            (stream->list
             (stream-filter pair? mixed-stream)))])
      sorted-ID-ranges)))
     

      
(define (combine-ranges ranges)
  (define (iter result-so-far current-range remaining-ranges)
    (cond [(null? remaining-ranges)
           (sort-ranges
            (if (null? current-range)
                result-so-far
                (cons current-range result-so-far)))]

          [else
           (let ([next-range (car remaining-ranges)]
                 [new-remaining-ranges (cdr remaining-ranges)])
             (if (null? current-range)
                 (iter result-so-far next-range new-remaining-ranges)
                 (let ([current-lo (car current-range)]
                       [current-hi (cdr current-range)]
                       [next-lo (car next-range)]
                       [next-hi (cdr next-range)])
                   (cond
                     [(> next-lo current-hi) ; no overlap
                      (iter
                       (cons current-range result-so-far)
                       next-range
                       new-remaining-ranges)]
                     
                     [(> next-hi current-hi) ; overlap, discard current-hi
                      (iter
                       result-so-far
                       (cons current-lo next-hi)
                       new-remaining-ranges)]
                     
                     [else ; overlap, discard next-hi
                      (iter
                       result-so-far
                       (cons current-lo current-hi)
                       new-remaining-ranges)]))))]))

  (iter '() '() ranges))


(define (fresh-ID-count-by-ranges in-port)
  (let ([consolidated-ranges
         (combine-ranges
          (gather-and-sort-ranges in-port))])
    (for/fold ([sum 0])
              ([next-range consolidated-ranges])
      (+ sum
         (+
          (-
           (cdr next-range)
           (car next-range))
          1)))))
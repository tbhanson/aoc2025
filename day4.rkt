#lang racket

(require parser-tools/lex)


(provide
 (contract-out
  [read-forklift-grid (-> port? vector?)]
  [adjacent-roll-count (-> vector? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [accessible-roll-count (-> vector? exact-nonnegative-integer?)]

  [accessible-roll-at-xy? (-> vector? exact-nonnegative-integer? exact-nonnegative-integer? boolean?)]
  )
 )


(define forklift-grid-lexer
  (lexer
   [(concatenation (repetition 1 999 (char-set ".@")))
   
    (begin
      (stream-cons
       (list->vector
        (string->list lexeme))
       (forklift-grid-lexer input-port)))]
      
   [whitespace
    (forklift-grid-lexer input-port)]
   
   [(eof)
    empty-stream]
   ))

(define (read-forklift-grid a-port)
  (list->vector
   (stream->list
    (forklift-grid-lexer a-port))))

(define neighbors-relative
  (filter
   (lambda (coord)
     (not
      (and (= (car coord) 0)
           (= (cdr coord) 0))))
   
   (for*/list ([i (in-range -1 2)]
               [j (in-range -1 2)])
     (cons i j))))

(define (char-at a-grid coord)
  (let ([i (car coord)]
        [j (cdr coord)])
    (vector-ref
     (vector-ref a-grid i)
     j)))

(define (adjacent-roll-count a-grid row col)
  (let ([M (vector-length a-grid)]
        [N (vector-length (vector-ref a-grid 0))])

    (define (legal-coord? coord)
      (let ([i (car coord)]
            [j (cdr coord)])
        (and (<= 0 i) (< i M)
             (<= 0 j) (< j N))))

    (define (sum-coords c1 c2)
      (cons
       (+ (car c1) (car c2))
       (+ (cdr c1) (cdr c2))))

   
             
    (for/fold ([sum 0])
              ([neighbor
                (stream-filter
                 legal-coord?
                 (stream-map
                  (lambda (c)
                    (sum-coords (cons row col) c))
                  neighbors-relative))])
      (if (equal? (char-at a-grid neighbor) #\@)
          (+ sum 1)
          sum))))

(define (roll-at-xy? a-grid i j)
  (equal?
   (char-at a-grid (cons i j))
   #\@))

(define (xy-accessible? a-grid i j)
  (<
   (adjacent-roll-count a-grid i j)
   4))

(define (accessible-roll-count a-grid)
  (let ([M (vector-length a-grid)]
        [N (vector-length (vector-ref a-grid 0))])
    (for*/fold ([sum 0])
               ([i (in-range M)]
                [j (in-range N)])
      (if (and
           (roll-at-xy? a-grid i j)
           (xy-accessible? a-grid i j))
          (+ sum 1)
          sum))))
    

(define (accessible-roll-at-xy? a-grid x y)
  (and
   (roll-at-xy? a-grid x y)
   (xy-accessible? a-grid x y)))
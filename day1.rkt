#lang racket

(require parser-tools/lex)

(struct lock-rotation
  (direction how-far)
  #:prefab
  )

(provide
 (contract-out
  ; struct automatics
  [lock-rotation? (-> any/c boolean?)]
  [lock-rotation (-> (or/c #\R #\L) exact-nonnegative-integer? lock-rotation?)]
  [lock-rotation-direction (-> lock-rotation? (or/c #\R #\L))]
  [lock-rotation-how-far (-> lock-rotation? exact-nonnegative-integer?)]
  ;
  [read-lock-rotations (-> port? stream?)]
  [next-position (-> exact-nonnegative-integer? lock-rotation? exact-nonnegative-integer?)]
  [get-password-from-lock-rotations-part1 (-> stream? exact-nonnegative-integer?)]
  [next-position-and-zero-count (-> exact-nonnegative-integer? lock-rotation? (values exact-nonnegative-integer? exact-nonnegative-integer? ))]
  [get-password-from-lock-rotations-part2 (-> stream? exact-nonnegative-integer?)]
  )
 )

(define (string->lock-roation str)
  (lock-rotation (string-ref str 0) (string->number (substring str 1))))

(define lock-rotation-lexer
  (lexer
   [(concatenation (char-set "RL") (repetition 1 99 (char-set "0123456789")))
   
    (begin
      ;(printf "saw lock-rotation: ~a~n" lexeme)
      (stream-cons
       (string->lock-roation lexeme)
       (lock-rotation-lexer input-port)))]
      
   [(union #\space #\newline #\tab)
    (lock-rotation-lexer input-port)]

   [(eof)
    empty-stream]
   ))

(define (read-lock-rotations a-port)
   (lock-rotation-lexer a-port))

; in some sense we solved the first part with some optimization
(define (next-position current-position next-lock-rotation)
  (let ([how-far-mod-100 (remainder (lock-rotation-how-far next-lock-rotation) 100)])
    (let ([add-to-position
           (if (equal? (lock-rotation-direction next-lock-rotation) #\R)
               how-far-mod-100
               (- 0 how-far-mod-100))])
      (let ([almost-next-position
             (+ current-position add-to-position)])
        (if (< almost-next-position 0)
            (+ 100 almost-next-position)
            (remainder almost-next-position 100))))))

; it might make sense to solve the second part more naively: tick positions one at a time and count zeroes as we go
(define (next-position-and-zero-count current-position next-lock-rotation)
  (define (iter pos-so-far zero-count-so-far rotation-remaining)
    (cond [(= rotation-remaining 0)
           (values pos-so-far zero-count-so-far)]
          [(< rotation-remaining 0) ; click one to the left
           (let ([new-pos-almost (- pos-so-far 1)])
             (let ([new-pos
                    (if (< new-pos-almost 0)
                        (+ 100 new-pos-almost)
                        new-pos-almost)])
               (let ([zero-count-increment
                      (if (= new-pos 0)
                          1
                          0)])
                 (iter new-pos (+ zero-count-so-far zero-count-increment) (+ rotation-remaining 1)))))]
          [else ; click one to the right
           (let ([new-pos (remainder (+ pos-so-far 1) 100)])
             (let ([zero-count-increment
                    (if (= new-pos 0)
                        1
                        0)])
               (iter new-pos (+ zero-count-so-far zero-count-increment) (- rotation-remaining 1))))]))

  (if (equal? (lock-rotation-direction next-lock-rotation) #\R)
      (iter current-position 0 (lock-rotation-how-far next-lock-rotation))       ; right is positive
      (iter current-position 0 (- 0 (lock-rotation-how-far next-lock-rotation))) ; left is negative
      ))

                  
              


(define (get-password-from-lock-rotations-part1 stream-of-lock-rotations)
  (let-values ([(answer final-pos)
                (for/fold ([result 0]  ; start zero count at 0
                           [current-pos 50] ; start position at 50
                           )
                          ([next-lock-rotation stream-of-lock-rotations])
                  (let ([new-pos (next-position current-pos next-lock-rotation)])
                    (if (= new-pos 0)
                        (values (+ result 1) new-pos)
                        (values result new-pos))))])
    answer))

(define (get-password-from-lock-rotations-part2 stream-of-lock-rotations)
  (let-values ([(answer final-pos)
                (for/fold ([result 0]  ; start zero count at 0
                           [current-pos 50] ; start position at 50
                           )
                          ([next-lock-rotation stream-of-lock-rotations])
                  (let-values ([(new-pos zero-count-inc)
                                (next-position-and-zero-count current-pos next-lock-rotation)])
                    (values (+ result zero-count-inc) new-pos)))])
    answer))

      
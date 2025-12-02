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
  [get-password-from-lock-rotations (-> stream? exact-nonnegative-integer?)]
  )
 )

(define (string->lock-roation str)
  ;(assert (= (string-length str) 2) (format "poker-card string should be exactly 2 characters long (~a)~n" str))
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
             

(define (get-password-from-lock-rotations stream-of-lock-rotations)
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
          
      
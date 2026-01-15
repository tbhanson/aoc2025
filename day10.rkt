#lang racket


(provide
 (contract-out
  ;part 1
  [read-manual-line-bits (-> port? stream?)]
  ))

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))

(define (read-manual-line-bits in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (begin
          ;(printf "next-line: ~a~n" next-line)
          (let ([next-bit
                 (regexp-match
                  #rx"([[][.#]+[]]) ([ ()0-9,]+) ([{][0-9,]+[}])"
                  next-line)])
           
            ;(printf "next-bit: ~a~n" next-bit)
            (assert
             (string=?
              (car next-bit)
              next-line)
             (format "hmmm, we expected to match the whole line (~a), but apparently got less (~a)~n"
                     next-line
                     (car next-bit)))
            
            (assert
             (= (length next-bit) 4)
             (format "hmmm, we expected to match the whole line (~a) in 3 parts, but apparently got a different number (~a)~n"
                     next-line
                     (- (length next-bit) 1)))
            
             
            (stream-cons next-bit (read-manual-line-bits in-port)))))))


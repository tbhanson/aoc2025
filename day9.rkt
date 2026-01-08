#lang racket


(provide
 (contract-out
  ;part 1
  [read-positions (-> port? stream?)]
  [max-rectangle (-> port? exact-nonnegative-integer?)]
  [green-from-to (-> pair? pair? set?)]
  [boundary-green-tiles (-> port? set?)]
  [max-rectangle-part-2 (-> port? exact-nonnegative-integer?)]
  ))

(define (read-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (let ([next-pair
                 (cons
                  (car next-tuple)
                  (cadr next-tuple))])
            (stream-cons next-pair (read-positions in-port)))))))

(define (max-rectangle in-port)
  (let ([all-corners (read-positions in-port)])
    (for*/fold ([max-rectangle-size 0])
               ([c1 all-corners]
                [c2 all-corners]
                #:unless (equal? c1 c2))
      ;(printf "c1: ~a; c2: ~a~n" c1 c2)
      (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
            [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
        ;(printf "x-diff: ~a; y-diff: ~a~n" x-diff y-diff)
        (if (> (* x-diff y-diff) max-rectangle-size)
            (* x-diff y-diff)
            max-rectangle-size)))))
          
(define (green-from-to p1 p2)
  (define (assert pred anError)
    (if (not pred) 
        (error anError)
        #t))
  
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
      
    (if (= x1 x2)
        (let ([lo-y (min y1 y2)]
              [hi-y (max y1 y2)])
          (for/fold ([result (set)])
                    ([y (in-range (+ lo-y 1) hi-y)])
            (set-add result (cons x1 y))))
        (begin
          (assert (= y1 y2)
                  (format "hmmm, we think either x1 = x2 or y1 = y2 must obtain! (~a ~a), (~a ~a)" x1 y1 x2 y2))
          (let ([lo-x (min x1 x2)]
                [hi-x (max x1 x2)])
            (for/fold ([result (set)])
                      ([x (in-range (+ lo-x 1) hi-x)])
              (set-add result (cons x y1))))))))
        
        
  

(define (boundary-green-tiles in-port)
  (let ([all-corners (read-positions in-port)])
    (let ([first-last (stream-first all-corners)])
      (define (iter green-so-far previous-corner remaining-corners)
        (if (stream-empty? remaining-corners)
            (set-union green-so-far (green-from-to previous-corner first-last))
            (let ([next-corner (stream-first remaining-corners)]
                  [new-remaining (stream-rest remaining-corners)])
              (iter
               (set-union green-so-far (green-from-to previous-corner next-corner))
               next-corner
               new-remaining))))

      (iter (set) first-last (stream-rest all-corners)))))

      
      
  
(define (max-rectangle-part-2 in-port)
  0)
    

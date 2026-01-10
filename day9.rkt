#lang racket


(provide
 (contract-out
  ;part 1
  [read-corner-positions (-> port? stream?)]
  [max-rectangle (-> port? exact-nonnegative-integer?)]
  [green-from-to (-> pair? pair? set?)]
  [get-red-corners (-> stream? set?)]
  [get-boundary-green-tiles (-> stream? set?)]
  [get-internal-green-tiles (-> stream? set?)]
  [max-rectangle-part-2 (-> port? exact-nonnegative-integer?)]
  ))

(define (read-corner-positions in-port)
  (let ([next-line (read-line in-port)])
    (if (eof-object? next-line)
        empty-stream
        (let ([next-tuple
               (map string->number (string-split next-line ","))])
          (let ([next-pair
                 (cons
                  (car next-tuple)
                  (cadr next-tuple))])
            (stream-cons next-pair (read-corner-positions in-port)))))))

(define (max-rectangle in-port)
  (let ([all-corners (read-corner-positions in-port)])
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
        
        
(define (get-red-corners corner-positions)
  (list->set
   (stream->list corner-positions)))

  

(define (get-boundary-green-tiles corner-positions)
  (let ([first-last (stream-first corner-positions)])
    (define (iter green-so-far previous-corner remaining-corners)
      (if (stream-empty? remaining-corners)
          (set-union green-so-far (green-from-to previous-corner first-last))
          (let ([next-corner (stream-first remaining-corners)]
                [new-remaining (stream-rest remaining-corners)])
            (iter
             (set-union green-so-far (green-from-to previous-corner next-corner))
             next-corner
             new-remaining))))

    (iter (set) first-last (stream-rest corner-positions))))


; not quite sure how to do this!
; one way might be
; 1) to find any interior point (e.g. an uncolored one adjacent to a green border from which we can't get to an outer edge via open squares)
; 2) call this point the frontier in a kind of A* algorithm that converts uncolored points to green until it reaches green or red points
;
; another way might be to sweep the entire field from left to right or top to bottom, counting "border crossings" and marking points interior which
; are an odd number of crossings "in"
;
; the second approach feels easier, but I'm not sure.
;
; now I don't think the second approach works, at least not as I naively planned: 
;
(define (get-internal-green-tiles corner-positions)
  (let ([red-corners (get-red-corners corner-positions)]
        [boundary-green-tiles (get-boundary-green-tiles corner-positions)])
    (printf "count red-corners: ~a~n" (set-count red-corners))
    (printf "count boundary-green-tiles ~a~n" (set-count boundary-green-tiles))

    (define (internal-green-row-x x)
      (define (iter green-so-far greens reds state pos max-pos)
        (if (>= pos max-pos)
            green-so-far
            (let ([pos-color
            (cond [(= state 'outside)

              
            
      (printf "(internal-green-row-x ~a)~n" x)
      (let ([points-this-row
             (stream-filter
              (lambda (point) (= (car point) x))
              (set->stream
               (set-union red-corners boundary-green-tiles)))])
        ;(printf " points-this-row: ~a~n" (stream->list points-this-row))
        (let ([points-this-row-sorted
               (sort
                (stream->list points-this-row)
                (lambda (p1 p2) (< (cdr p1) (cdr p2))))])
          (printf " points-this-row-sorted: ~a~n" points-this-row-sorted)
          (set))))
    
    (let ([max-x (for/fold ([result 0])
                           ([next-x (stream-map car (set->stream red-corners))])
                   (if (> next-x result) next-x result))]
          [max-y (for/fold ([result 0])
                           ([next-y (stream-map cdr (set->stream red-corners))])
                   (if (> next-y result) next-y result))])
      (printf "max-x: ~a~n" max-x)
      (printf "max-y: ~a~n" max-y)
      (for/fold ([result (set)])
                ([x (in-range (+ max-x 1))])
        (set-union result (internal-green-row-x x))))))
    

  
(define (max-rectangle-part-2 in-port)
  0)
    

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
  [get-red-and-green-tiles (-> stream? set?)]
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

(define (assert pred anError)
  (if (not pred) 
      (error anError)
      #t))
  

(define (green-from-to p1 p2)
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


; 1) find any interior point (e.g. to the right of a green border at the right edge (with minimal x value))
; 2) call this point the frontier in a kind of A* algorithm that converts uncolored points to green until it reaches green or red points

(define (get-internal-green-tiles corner-positions)
  (let ([red-corners (get-red-corners corner-positions)]
        [boundary-green-tiles (get-boundary-green-tiles corner-positions)])
    (printf "count red-corners: ~a~n" (set-count red-corners))
    (printf "count boundary-green-tiles ~a~n" (set-count boundary-green-tiles))
    (let ([boundary-tiles (set-union red-corners boundary-green-tiles)])
      
      (define (find-internal-point)
        ;(printf "(find-internal-point)~n")
        (let ([green-min-x
               (for/fold ([result 99999])
                         ([next-green-x
                           (stream-map car (set->stream boundary-green-tiles))])
                 (if (< next-green-x result) next-green-x result))])
 
          (let ([left-edge-green-tiles
                 (stream-filter
                  (lambda (point) (= (car point) green-min-x))
                  (set->stream boundary-green-tiles))])
                    
            ;(printf " left-edge-green-tiles: ~a~n" left-edge-green-tiles)
            (let ([first-left-green (stream-first left-edge-green-tiles)])
              (let ([hopefully-internal-point
                     (cons (+ 1 (car first-left-green)) (cdr first-left-green))])
                (assert (not (set-member? boundary-tiles hopefully-internal-point))
                        (format "autschn! our candidate interior point wasn't supposed to be a boundary point, but alas it is! (~a)" hopefully-internal-point))
                
                ;(printf " hopefully-internal-point: ~a~n" hopefully-internal-point)
                hopefully-internal-point)))))

      ; iterate until we've collected all interior points
      (define (iter interior-so-far explore-further)
        (define (left-of-me p) (cons (- (car p) 1) (cdr p)))
        (define (right-of-me p) (cons (+ (car p) 1) (cdr p)))
        (define (above-me p) (cons (car p) (- (cdr p) 1)))
        (define (below-me p) (cons (car p) (+ (cdr p) 1)))
        
        (if (null? explore-further)
            interior-so-far
            (let ([next-interior (car explore-further)]
                  [still-to-explore (cdr explore-further)])
              (let ([neighbors
                     (list
                      (left-of-me next-interior)
                      (right-of-me next-interior)
                      (above-me next-interior)
                      (below-me next-interior))])
                (let ([eligible
                       (filter
                        (lambda (p)
                          (not (set-member?
                                (set-union boundary-tiles interior-so-far)
                                p)))
                        neighbors)])
                  (iter
                   (set-add interior-so-far next-interior)
                   (append eligible still-to-explore)))))))

      (iter (set) (list (find-internal-point))))))
                           
(define (get-red-and-green-tiles corner-positions)
  (let ([red-corners (get-red-corners corner-positions)]
        [boundary-green-tiles (get-boundary-green-tiles corner-positions)]
        [inner-green-tiles (get-internal-green-tiles corner-positions)])
    (printf "count inner-green-tiles ~a~n" (set-count inner-green-tiles))
    (set-union red-corners boundary-green-tiles inner-green-tiles)))


(define (max-rectangle-part-2 in-port)
  (let ([all-corners (read-corner-positions in-port)])
    (let ([red-and-green-tiles
           (get-red-and-green-tiles all-corners)])
    
      (define (contains-non-red-or-green? p1 p2)
        (let ([stream-of-rectangle-points
               (let ([x1 (car p1)]
                     [y1 (cdr p1)]
                     [x2 (car p2)]
                     [y2 (cdr p2)])
                 (let ([lo-x (min x1 x2)]
                       [hi-x (max x1 x2)]
                       [lo-y (min y1 y2)]
                       [hi-y (max y1 y2)])
                   (for*/stream ([x (in-range lo-x (+ hi-x 1))]
                                 [y (in-range lo-y (+ hi-y 1))])
                     (cons x y))))])
          (not
           (stream-andmap
            (lambda (point) (set-member? red-and-green-tiles point))
            stream-of-rectangle-points))))
         
      (for*/fold ([max-rectangle-size 0])
                 ([c1 all-corners]
                  [c2 all-corners]
                  #:unless (equal? c1 c2)
                  #:unless (contains-non-red-or-green? c1 c2))
                
        ;(printf "c1: ~a; c2: ~a~n" c1 c2)
        (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
              [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
          ;(printf "x-diff: ~a; y-diff: ~a~n" x-diff y-diff)
          (if (> (* x-diff y-diff) max-rectangle-size)
              (begin
                (printf "found bigger:  x-diff: ~a y-diff: ~a~n" x-diff y-diff)
                (* x-diff y-diff))
              max-rectangle-size))))))
  

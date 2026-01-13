#lang racket


(provide
 (contract-out
  ;part 1
  [read-corner-positions (-> port? stream?)]
  [max-rectangle (-> port? exact-nonnegative-integer?)]
  [green-from-to (-> pair? pair? set?)]
  [get-red-corners (-> stream? set?)]
  [get-boundary-green-tiles (-> stream? set?)]
  [choose-external-point-from-directed-segment (-> pair? pair? pair?)] ; pair of points -> one point (pair of coordinates)
  [get-external-points-adjacent-to-boundary (-> stream? set?)]
  [get-rectangles-by-size-descending (-> stream? list?)]
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
          (cond [(<= (- hi-y lo-y) 1)
                 (printf "somewhat surprisingly, perhaps, ~a and ~a are only ~a apart in y direction~n" p1 p2 (- hi-y lo-y))])
                
          (for/fold ([result (set)])
                    ([y (in-range (+ lo-y 1) hi-y)])
            (set-add result (cons x1 y))))
        (begin
          (assert (= y1 y2)
                  (format "hmmm, we think either x1 = x2 or y1 = y2 must obtain! (~a ~a), (~a ~a)" x1 y1 x2 y2))
          (let ([lo-x (min x1 x2)]
                [hi-x (max x1 x2)])
            (cond [(<= (- hi-x lo-x) 1)
                   (printf "somewhat surprisingly, perhaps, ~a and ~a are only ~a apart in x direction~n" p1 p2 (- hi-x lo-x))])

            (for/fold ([result (set)])
                      ([x (in-range (+ lo-x 1) hi-x)])
              (set-add result (cons x y1))))))))


(define (choose-external-point-from-directed-segment p1 p2)
  ;(printf "(choose-external-point-from-directed-segment ~a ~a)~n" p1 p2)
    
  (let ([p2-p1
         (cons
          (- (car p2) (car p1))
          (- (cdr p2) (cdr p1)))])
    ;(printf " p2-p1: ~a~n" p2-p1)
    
    (let ([direction
           (cond [(= (car p2-p1) 0)
                  (cons 0  (/ (cdr p2-p1) (abs (cdr p2-p1))))]
                 [(= (cdr p2-p1) 0)
                  (cons (/ (car p2-p1) (abs (car p2-p1))) 0)]
                 [else
                  (error (format "one of the two coordinates (not both) must not change (~a -> ~a)" p1 p2))])])
      ;(printf " direction: ~a~n" direction)
      
      (let ([half-way
             (if (= (car p2-p1) 0)
                 (cons (car p1) (+ (cdr p1) (floor (/ (cdr p2-p1) 2))))
                 (cons (+ (car p1) (floor (/ (car p2-p1) 2))) (cdr p1)))])
        ;(printf " half-way: ~a~n" half-way)
  
        (let ([result
               (cond [(equal? direction (cons 0 1))
                      (cons (- (car half-way) 1) (cdr half-way))]
                     [(equal? direction (cons 0 -1))
                      (cons (+ (car half-way) 1) (cdr half-way))]
                     [(equal? direction (cons 1 0))
                      (cons (car half-way) (+ (cdr half-way) 1))]
                     [(equal? direction (cons -1 0))
                      (cons (car half-way) (- (cdr half-way) 1))]
                     [else
                      (error (format "we thought direction could be only one of 4; what is this: ~a ?!" direction))])])
          result)))))
                     

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

; what about:
; 1) compute a set of outside points adjacent to green borders (how hard is this? find the green border with minimal x value, choose an adjacent point with x-1, trace around the figure?)
; 2) compute rectangles (as in part 1) by size descending
; 3) filter these, rejecting those that contain any outside point; the first remaining should be the one we want

; if we do it, right, it feels as though this can be computed much like get-boundary-green-tiles was
; do we first rotate the ring of corner points so we start at those with minimal x ?

; actually we shouldn't need to, but success depends on whether they visit the points in a clockwise or counterclockwise direction! (I think)
; if they go clockwise, we should compute all external points (counterclockwise all internal)
; we could compute one way, then make sure no rectangle contains any of these points (?)
; if it does, we could reverse the list and do it over (or reverse our rules, but that sounds harder)

(define (get-external-points-adjacent-to-boundary corner-positions)
  ;(printf "(get-external-points-adjacent-to-boundary (stream) ~a)~n" (stream->list corner-positions))
  
  (let ([first-last (stream-first corner-positions)])
    
    (define (iter externals-so-far previous-corner remaining-corners)
      ;(printf "(iter ~a ~a (stream) ~a)~n" externals-so-far previous-corner (stream->list remaining-corners))
      
      (if (stream-empty? remaining-corners)
          (set-add externals-so-far (choose-external-point-from-directed-segment previous-corner first-last))
          (let ([next-corner (stream-first remaining-corners)]
                [new-remaining (stream-rest remaining-corners)])
            (iter
             (set-add externals-so-far (choose-external-point-from-directed-segment previous-corner next-corner))
             next-corner
             new-remaining))))

    (iter (set) first-last (stream-rest corner-positions))))

; produce entries that look like this: (list 50 (cons 2 5) (cons 11 1))
; that is, size, p1, p2

(define (get-rectangles-by-size-descending all-corners)
  (sort
   (for/fold ([result '()])
             ([c1 all-corners]
              [i1 (in-range (stream-length all-corners))])
     (append
      result
      (for/list
          ([c2 all-corners]
           #:unless (equal? c1 c2))
        (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
              [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
          (let ([rect-size (* x-diff y-diff)])
            (list rect-size c1 c2))))))

   (lambda (size-r1 size-r2)
     (> (car size-r1)  (car size-r2)))))


(define (ref-point-in-rectangle? ref p1 p2)
  ;(printf "(ref-point-in-rectangle? ~a ~a ~a)~n" ref p1 p2)
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
      
    (let ([lo-x (min x1 x2)]
          [hi-x (max x1 x2)]
          [lo-y (min y1 y2)]
          [hi-y (max y1 y2)])
      (let ([result
             (and
              (<= lo-x (car ref))
              (<= (car ref) hi-x)
              (<= lo-y (cdr ref))
              (<= (cdr ref) hi-y))])
        ;(printf " : ~a~n" result)
        result))))
       
       


(define (max-rectangle-part-2 in-port)
  (let ([all-corners (read-corner-positions in-port)])
    (let ([rectangles-by-size-descending (get-rectangles-by-size-descending all-corners)]
          [external-points-adjacent-to-boundary
           (set->list
            (get-external-points-adjacent-to-boundary all-corners))])

      ; stream-ormap seemed more efficient but seemed only to examine one point (!?), so for now use this slower method
      (define (rectangle-contains-no-external-points sized-rectangle)
        ;(printf "(rectangle-contains-no-external-points ~a)~n" sized-rectangle)
        ;(printf "  (checking against ~a external points)~n" (length external-points-adjacent-to-boundary))
        (let ([p1 (cadr sized-rectangle)]
              [p2 (caddr sized-rectangle)])
          (let ([result
                 (for/fold ([total-result #t])
                           ([ext-pt external-points-adjacent-to-boundary]
                            [counter (in-naturals 1)])
                                    
                   (begin
                     ;(printf " ~a: (ref-point-in-rectangle? ~a ~a ~a)~n" counter ext-pt p1 p2)
                     (let ([result
                            (not (ref-point-in-rectangle? ext-pt p1 p2))])
                       ;(printf "  ~a~n" result)
                       (and total-result result))))])
            ;(printf " result: ~a~n" result)
            result)))

      (define (iter rectangles-not-yet-checked)
        ;(printf "- - -~n")
        (if (null? rectangles-not-yet-checked)
            (error "oh, dear, we didn't find ANY rectangles that satisfy our constraints")
            (let ([next-rectangle-to-check (car rectangles-not-yet-checked)]
                  [remaining-rectangles-to-check (cdr rectangles-not-yet-checked)])
              (if (rectangle-contains-no-external-points next-rectangle-to-check)
                  (begin
                    (printf "yippee! rectangle ~a is the one!~n" next-rectangle-to-check)
                    (car next-rectangle-to-check) ; return size as promised
                    )
                  (iter remaining-rectangles-to-check)))))

      (printf "count of all-corners: ~a~n" (stream-length all-corners))
      (printf "count of external-points-adjacent-to-boundary: ~a~n" (set-count external-points-adjacent-to-boundary))
      (printf "count of rectangles-by-size-descending ~a~n" (length rectangles-by-size-descending))
      
      (iter rectangles-by-size-descending))))
              


    
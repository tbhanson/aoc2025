#lang racket


(provide
 (contract-out
  ;part 1
  [read-corner-positions (-> port? stream?)]
  [max-rectangle (-> port? exact-nonnegative-integer?)]
  [green-from-to (-> pair? pair? set?)]
  [get-red-corners (-> stream? set?)]
  [get-boundary-green-tiles (-> stream? set?)]
  ;[get-internal-green-tiles (-> stream? set?)]
  ;[get-red-and-green-tiles (-> stream? set?)]
  [get-rectangles-by-size-descending (-> port? list?)]
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

; what about:
; 1) compute a set of outside points adjacent to green borders (how hard is this? find the green border with minimal x value, choose an adjacent point with x-1, trace around the figure?)
; 2) compute rectangles (as in part 1) by size descending
; 3) filter these, rejecting those that contain any outside point; the first remaining should be the one we want

(define (get-rectangles-by-size-descending in-port)
  (let ([all-corners (read-corner-positions in-port)])
    (sort
     (for/fold ([result '()])
               ([c1 all-corners]
                [i1 (in-range (stream-length all-corners))])
       (append
        result
        (for/list
            ([c2 all-corners]
             [i2 (in-range (stream-length all-corners))]
             #:unless (>= i2 i1))
          (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
                [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
            (let ([rect-size (* x-diff y-diff)])
              (list rect-size c1 c2))))))

     (lambda (size-r1 size-r2)
       (> (car size-r1)  (car size-r2))))))

(define (max-rectangle-part-2 in-port)
  0)


;;            
;;             (let ([all-corners (read-corner-positions in-port)])
;;               (let ([red-and-green-tiles
;;                      (get-red-and-green-tiles all-corners)])
;;     
;;                 (define (contains-non-red-or-green? p1 p2)
;;                   (let ([stream-of-rectangle-points
;;                          (let ([x1 (car p1)]
;;                                [y1 (cdr p1)]
;;                                [x2 (car p2)]
;;                                [y2 (cdr p2)])
;;                            (let ([lo-x (min x1 x2)]
;;                                  [hi-x (max x1 x2)]
;;                                  [lo-y (min y1 y2)]
;;                                  [hi-y (max y1 y2)])
;;                              (for*/stream ([x (in-range lo-x (+ hi-x 1))]
;;                                            [y (in-range lo-y (+ hi-y 1))])
;;                                (cons x y))))])
;;                     (not
;;                      (stream-andmap
;;                       (lambda (point) (set-member? red-and-green-tiles point))
;;                       stream-of-rectangle-points))))
;;          
;;                 (for*/fold ([max-rectangle-size 0])
;;                            ([c1 all-corners]
;;                             [c2 all-corners]
;;                             #:unless (equal? c1 c2)
;;                             #:unless (contains-non-red-or-green? c1 c2))
;;                 
;;                   ;(printf "c1: ~a; c2: ~a~n" c1 c2)
;;                   (let ([x-diff (+ 1 (abs (- (car c1) (car c2))))]
;;                         [y-diff (+ 1 (abs (- (cdr c1) (cdr c2))))])
;;                     ;(printf "x-diff: ~a; y-diff: ~a~n" x-diff y-diff)
;;                     (if (> (* x-diff y-diff) max-rectangle-size)
;;                         (begin
;;                           ;(printf "found bigger:  x-diff: ~a y-diff: ~a~n" x-diff y-diff)
;;                           (* x-diff y-diff))
;;                         max-rectangle-size))))))
;;   
;;           
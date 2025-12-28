#lang racket

(require parser-tools/lex)
(require math/array)

(struct path-tree
  (coordinate left-child right-child)
  #:prefab
  )

(provide
 (contract-out
  ;part 1
  [read-manifold (-> port? vector?)]
  [S-coord-of-manifold (-> vector? pair?)]
  [splits-in-manifold (-> vector? exact-nonnegative-integer?)]
  ;part 2
  ; struct automatics
  [path-tree? (-> any/c boolean?)]
  [path-tree (-> pair? (or/c path-tree? #f) (or/c path-tree? #f) path-tree?)]
  [path-tree-coordinate (-> path-tree? pair?)]
  [path-tree-left-child (-> path-tree? (or/c path-tree? #f))]
  [path-tree-right-child (-> path-tree? (or/c path-tree? #f))]
  ;
  [tree-of-paths-in-manifold (-> vector? (or/c path-tree? #f))]
  [timelines-of-splits-in-manifold (-> vector? exact-nonnegative-integer?)]
  ))

(define (read-manifold in-port)
  (define (read-lines a-port)
    (let ([next-line (read-line a-port)])
      (if (eof-object? next-line)
          empty-stream
          (stream-cons next-line (read-lines a-port)))))

  (for/vector ([line (read-lines in-port)])
    (list->vector
     (string->list line))))

(define (unique a-list)
  (cond [(= (length a-list) 1)
         (car a-list)]
        [else
         (error
          (format "unique demands precisely 1 element, but got ~a" a-list))]))
         
(define (S-coord-of-manifold manifold)
  (let ([row-count (vector-length manifold)]
        [col-count (vector-length (vector-ref manifold 0))])
    (unique
     (for*/fold ([S-coords '()])
                ([row (in-range row-count)]
                 [col (in-range col-count)])
       (if (char=?
            (vector-ref (vector-ref manifold row) col)
            #\S)
           (cons
            (cons row col)
            S-coords)
           S-coords)))))

(define (splits-in-manifold manifold)
  (define (compute-possible-tachyon-splits tachyons)
    (define (iter new-so-far splits-so-far remaining)
      (if (null? remaining)
          (values new-so-far splits-so-far)
          (let ([next (car remaining)]
                [rest (cdr remaining)])
            (cond [(char=?
                    (vector-ref (vector-ref manifold (car next)) (cdr next))
                    #\.) ; no split
                   (iter (cons next new-so-far) splits-so-far rest)]

                  [(char=?
                    (vector-ref (vector-ref manifold (car next)) (cdr next))
                    #\^) ; split!
                   (let ([left (cons (car next) (- (cdr next) 1))]
                         [right (cons (car next) (+ (cdr next) 1))])
                     (iter (cons left (cons right new-so-far))
                           (+ splits-so-far 1)
                           rest))]

                  [else
                   (error
                    (format
                     "we expect . or ^ in every cell a tachyon enters, but found ~a at (~a ~a)" 
                     (vector-ref (vector-ref manifold (car next)) (cdr next))
                     (car next)
                     (cdr next)))]))))
    (let-values ([(coords splits)
                  (iter '() 0 tachyons)])
      (values
       (set->list 
        (list->set
         coords)) ; eliminate duplicates
       splits)))
  
  (let ([row-count (vector-length manifold)]
        [S-coord (S-coord-of-manifold manifold)])
    (let-values
        ([(locations splits)
          (for/fold ([tachyon-locations (list S-coord)]
                     [split-count 0])
                    ([row (in-range (car S-coord) (- row-count 1))])
            ;;             (printf "tachyon-locations: ~a~n" tachyon-locations)
            ;;             (printf "split-count: ~a~n" split-count)
            ;;             (printf "row: ~a~n" row)
            
            (let ([tachyons-one-row-down
                   (map
                    (lambda (coord)
                      (cons (+ (car coord) 1) (cdr coord)))
                    tachyon-locations)])
              (let-values ([(next-tachyon-locations next-splits)
                            (compute-possible-tachyon-splits tachyons-one-row-down)])
                ;(printf "next-splits: ~a~n" next-splits)
                (let ([new-split-count
                       (+
                        split-count
                        next-splits)])
                  (values
                   next-tachyon-locations
                   new-split-count)))))])
      splits)))

; part 2
(define (tree-of-paths-in-manifold manifold)
  (let ([row-count (vector-length manifold)]
        [col-count (vector-length (vector-ref manifold 0))]
        [S-coord (S-coord-of-manifold manifold)])
    
    (define (tree-from coord-in-manifold)
      ;(printf "(tree-from ~a)~n" coord-in-manifold)
      
      (let ([row (car coord-in-manifold)]
            [col (cdr coord-in-manifold)])
        
        (if (>= row row-count) ; we've reached bottom: return a leaf
            (path-tree coord-in-manifold #f #f)
            (let ([char-at-coord
                   (vector-ref (vector-ref manifold row) col)])
               
              ;(printf " char-at-coord: ~a~n" char-at-coord)

              (cond
                ; no split
                [(or
                  (char=? char-at-coord #\.)
                  (char=? char-at-coord #\S))
                 (tree-from (cons (+ row 1) col))]

                ; split!
                [(char=?
                  char-at-coord
                  #\^)
                 (path-tree
                  coord-in-manifold
                  (tree-from (cons (+ row 1) (- col 1)))
                  (tree-from (cons (+ row 1) (+ col 1))))]
            
                [else
                 (error
                  (format
                   "we expect . or ^ in every cell a tachyon could enter, but found ~a at (~a ~a)"
                   char-at-coord
                   row col))])))))

      (tree-from S-coord)))

(define (timelines-of-splits-in-manifold manifold)
  (define (count-leaves tree)
    (define (leaf? tree)
      (and (not (path-tree-left-child tree))
           (not (path-tree-right-child tree))))
           
    (cond [(leaf? tree)
           1]
          [else
           (+ (count-leaves (path-tree-left-child tree))
              (count-leaves (path-tree-right-child tree)))]))
  
  (count-leaves (tree-of-paths-in-manifold manifold)))
   
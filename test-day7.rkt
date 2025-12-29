#lang racket

(require rackunit "day7.rkt")

(let ([sample-input
       (format
        (string-append
         ".......S.......~n"
         "...............~n"
         ".......^.......~n"
         "...............~n"
         "......^.^......~n"
         "...............~n"
         ".....^.^.^.....~n"
         "...............~n"
         "....^.^...^....~n"
         "...............~n"
         "...^.^...^.^...~n"
         "...............~n"
         "..^...^.....^..~n"
         "...............~n"
         ".^.^.^.^.^...^.~n"
         "...............~n"))])
  
  ; part 1
  (let ([in-port
         (open-input-string sample-input)])
    (let ([manifold
           (read-manifold in-port)])          
      (check-equal?
       (S-coord-of-manifold manifold)
       (cons 0 7))
      
      (check-equal?
       (splits-in-manifold manifold)
       21)
      ))
  
  ; do actual problem
  ;; (let ([in-port
  ;;        (open-input-file "test-data/input-day7-1.txt")])
  ;; 
  ;;   (let ([manifold
  ;;          (read-manifold in-port)])          
  ;; 
  ;;     (check-equal?
  ;;      (splits-in-manifold manifold)
  ;;      21)))
  
  ; part 2
  (let ([tree (path-tree (cons 3 3) #f #f)])
    (check-true
     (path-tree? tree)))

  (let ([simple-yields-empty
         (format
          (string-append
           "..S..~n"
           ".....~n"))])

    (let ([in-port
           (open-input-string simple-yields-empty)])
      (let ([manifold
             (read-manifold in-port)])          
        (check-equal?
         (tree-of-paths-in-manifold manifold)
         (path-tree (cons 2 2) #f #f))

        (check-equal?
         (timelines-of-splits-in-manifold manifold)
         1))))
    
  (let ([one-split
         (format
          (string-append
           "..S..~n"
           "..^..~n"
           ".....~n"))])

    (let ([in-port
           (open-input-string one-split)])
      (let ([manifold
             (read-manifold in-port)])          
        (check-equal?
         (tree-of-paths-in-manifold manifold)
         (path-tree (cons 1 2)
                    (path-tree (cons 3 1) #f #f)
                    (path-tree (cons 3 3) #f #f)
                    ))

        (check-equal?
         (timelines-of-splits-in-manifold manifold)
         2))))


  (let ([in-port
         (open-input-string sample-input)])
    (let ([manifold
           (read-manifold in-port)])          
      (check-equal?
       (timelines-of-splits-in-manifold manifold)
       40)))

  )
  
  

(let ([in-port
       (open-input-file "test-data/input-day7-10.txt")])
  
  (let ([manifold
         (read-manifold in-port)])          
    
    (check-equal?
     (timelines-of-splits-in-manifold manifold)
     13)))

(let ([in-port
       (open-input-file "test-data/input-day7-25.txt")])
  
  (let ([manifold
         (read-manifold in-port)])          
    
    (check-equal?
     (timelines-of-splits-in-manifold manifold)
     419)))

(time
 (let ([in-port
        (open-input-file "test-data/input-day7-50.txt")])
   
   (let ([manifold
          (read-manifold in-port)])          
     
     (check-equal?
      (timelines-of-splits-in-manifold manifold)
      103656)))
 )

;; ; cpu time: 31759 real time: 33392 gc time: 13525
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day7-75.txt")])
;;    
;;    (let ([manifold
;;           (read-manifold in-port)])          
;;      
;;      (check-equal?
;;       (timelines-of-splits-in-manifold manifold)
;;       26740010)))
;;  )
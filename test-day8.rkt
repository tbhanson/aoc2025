#lang racket

(require rackunit graph "day8.rkt")

(let ([sample-input
       (format
        (string-append
         "162,817,812~n"
         "57,618,57~n"
         "906,360,560~n"
         "592,479,940~n"
         "352,342,300~n"
         "466,668,158~n"
         "542,29,236~n"
         "431,825,988~n"
         "739,650,466~n"
         "52,470,668~n"
         "216,146,977~n"
         "819,987,18~n"
         "117,168,530~n"
         "805,96,715~n"
         "346,949,466~n"
         "970,615,88~n"
         "941,993,340~n"
         "862,61,35~n"
         "984,92,344~n"
         "425,690,689~n"))])
  (let ([in-port
         (open-input-string sample-input)])
    (let ([positions (read-positions in-port)])
           
      (check-equal?
       (stream-first
        positions)
       (list 162 817 812))

      ))
  
  (let ([in-port
         (open-input-string sample-input)])
    (let ([sample-world (read-point-world in-port)])
      (check-equal?
       (hash-ref
        (point-world-by-number sample-world)
        1)
       (list 162 817 812)
       )

      (check-equal?
       (hash-ref
        (point-world-by-position sample-world)
        (list 162 817 812))
       1
       )

      (let ([new-world (connect-closest-unconnected sample-world)])
        (check-equal?
         (list->set
          (get-vertices
           (point-world-connections new-world)))

         (set
          (list 162 817 812)
          (list 425 690 689)))

        (let ([new-new-world (connect-closest-unconnected new-world)])
          (check-equal?
           (list->set
            (get-vertices
             (point-world-connections new-new-world)))
           (set
            (list 162 817 812)
            (list 425 690 689)
            (list 431 825 988)))))

      
      ))

  (let ([in-port
         (open-input-string sample-input)])
    (let ([sample-world (read-point-world in-port)])
      (check-equal?
       (their-funny-product-after-N-iterations sample-world 10)
       40)
      )
    )

  ; some checks of timing

  ;;   ; cpu time: 4900 real time: 5129 gc time: 87
  ;;   (time
  ;;    (let ([in-port
  ;;           (open-input-file "test-data/input-day8-100.txt")])
  ;;    
  ;;      (let ([sample-world (read-point-world in-port)])
  ;;        (check-equal?
  ;;         (their-funny-product-after-N-iterations sample-world 100)
  ;;         990)
  ;;        )
  ;;      ))

  ;; (for ([in-file-name
  ;;        (list
  ;;         "test-data/input-day8-100.txt"
  ;;         "test-data/input-day8-200.txt"
  ;;         "test-data/input-day8-1.txt")])
  ;;   (time
  ;;    (let ([in-port
  ;;           (open-input-file in-file-name)])
  ;;      (let ([sample-world (read-point-world in-port)])
  ;;        (printf "read sample-world with ~a points and ~a distances~n"
  ;;                (length (hash-keys (point-world-by-number sample-world)))
  ;;                (length (point-world-sorted-distances sample-world)))))))
                 

  ; part 1 actual problem
  ; after improving my approach:
  ; cpu time: 838 real time: 884 gc time: 144
  ;; (time
  ;;  (let ([in-port
  ;;         (open-input-file "test-data/input-day8-1.txt")])
  ;;    
  ;;    (let ([sample-world (read-point-world in-port)])
  ;;      (check-equal?
  ;;       (their-funny-product-after-N-iterations sample-world 1000)
  ;;       ---)
  ;;      )
  ;;    ))

  ; part 2
  ;(printf "--------------------~n")
  ; small example
  (let ([in-port
         (open-input-string sample-input)])
    (let ([sample-world (read-point-world in-port)])
      ;(printf "sample-world: ~a~n" sample-world)

      
      (check-equal?
       (their-funny-product-after-all-connected sample-world)
       25272)
      )
    )
  )

; -200 -- cpu time: 4937 real time: 5086 gc time: 122
; -300 -- invariants on: cpu time: 38028 real time: 39126 gc time: 1741
; -300 -- invariants off: cpu time: 4286 real time: 4410 gc time: 123
; -400 -- invariants off: cpu time: 6723 real time: 6876 gc time: 234
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day8-400.txt")])
;;      
;;    (let ([sample-world (read-point-world in-port)])
;;      (check-equal?
;;       (their-funny-product-after-all-connected sample-world)
;;       8127628515)
;;      )
;;    ))

;; ; cpu time: 46743 real time: 47834 gc time: 2237
; on older puter: cpu time: 88983 real time: 90882 gc time: 2541
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day8-1.txt")])
;;      
;;    (let ([sample-world (read-point-world in-port)])
;;      (check-equal?
;;       (their-funny-product-after-all-connected sample-world)
;;       17)
;;      )
;;    ))

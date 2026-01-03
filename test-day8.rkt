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

      ; In this example, the two junction boxes which are closest together are 162,817,812 and 425,690,689.
      (check-equal?
       (closest-pair positions)
       (list->set
        (list
         (list 162 817 812)
         (list 425 690 689))))))

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
    ))

; part 1 actual problem

(time
 (let ([in-port
        (open-input-file "test-data/input-day8-200.txt")])
   
   (let ([sample-world (read-point-world in-port)])
     (check-equal?
      (their-funny-product-after-N-iterations sample-world 200)
      40)
     )
   ))
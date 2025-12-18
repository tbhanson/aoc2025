#lang racket

(require rackunit "day5.rkt")

; read input
(let ([in-port
       (open-input-string
        (format
         (string-append
          "3-5~n"
          "10-14~n"
          "16-20~n"
          "12-18~n~n"
          "1~n"
          "5~n"
          "8~n"
          "11~n"
          "17~n"
          "32~n")))])
          
  (let ([IDs-and-ranges
         (read-IDs-and-ranges in-port)])
    (let ([ID-ranges
           (stream-filter pair? IDs-and-ranges)]
          [IDs
           (stream-filter number? IDs-and-ranges)])
          
      (check-equal?
       (stream-first ID-ranges)
       (cons 3 5))
      
      (check-equal?
       (stream-first IDs)
       1)

      (check-equal?
       (count-of-fresh-IDs ID-ranges IDs)
       3))))

; do actual problem
;; (let ([in-port
;;        (open-input-file "test-data/input-day5-1.txt")])
;;   (let ([IDs-and-ranges
;;          (read-IDs-and-ranges in-port)])
;;     (check-equal?
;;      (input->fresh-ID-count IDs-and-ranges)
;;      ---)))

; part 2

(check-equal?
 (combine-ranges
  (list (cons 1 2)))
  (list (cons 1 2)))
  
(check-equal?
 (combine-ranges
  (list (cons 1 3) (cons 2 6) (cons 8 10) (cons 15 18)))
 (list (cons 1 6) (cons 8 10) (cons 15 18)))

(check-equal?
 (combine-ranges
  (list
   (cons 3 5)
   (cons 10 14)
   (cons 12 18)
   (cons 16 20)))
 (list
  (cons 3 5)
  (cons 10 20)))



(let ([their-small-input
       (format
        (string-append
         "3-5~n"
         "10-14~n"
         "16-20~n"
         "12-18~n~n"
         "1~n"
         "5~n"
         "8~n"
         "11~n"
         "17~n"
         "32~n"))])
  (let ([in-port
         (open-input-string their-small-input)])
    (check-equal?
     (gather-and-sort-ranges in-port)
     (list
      (cons 3 5)
      (cons 10 14)
      (cons 12 18)
      (cons 16 20))))
  
  (let ([in-port
         (open-input-string their-small-input)])
    (check-equal?
     (fresh-ID-count-by-ranges in-port)
     14)))



    


  
#lang racket

(require rackunit "day6.rkt")

; part 1
(let ([in-port
       (open-input-string
        (format
         (string-append
          "123 328  51 64~n"
          " 45 64  387 23~n"
          "  6 98  215 314~n"
          "*   +   *   + ~n")))])
          
  (let ([ceph-arith
         (read-cephalapod-arithmetic in-port)])          
    (check-equal?
     (stream-first ceph-arith)
     (list 123 328 51 64))

    (check-equal?
     (solve-cephalapod-arithmetic ceph-arith)
     4277556
    
    )))

; do actual problem
;; (let ([in-port
;;        (open-input-file "test-data/input-day6-1.txt")])
;; 
;;   (let ([ceph-arith
;;          (read-cephalapod-arithmetic in-port)])          
;;     (check-equal?
;;      (solve-cephalapod-arithmetic ceph-arith)
;;      ...
;;      )))

; part 2
(check-equal?
 (vector-vector-transpose
  #(#(1 2 3) #(4 5 6)))
 #(#(1 4) #(2 5) #(3 6)))

;; (let ([input-string
;;        (format
;;         (string-append
;;          "123 328  51 64 ~n"
;;          " 45 64  387 23 ~n"
;;          "  6 98  215 314~n"
;;          "*   +   *   +  ~n"))])
;; 
;;   (let ([in-port
;;          (open-input-string input-string)])
;;     (let ([line-stream (read-lines in-port)])
;;       (check-equal?
;;        (stream-length line-stream)
;;        4)
;;       (check-equal?
;;        (stream-first line-stream)
;;        "123 328  51 64 ")))
;; 
;;   (let ([in-port
;;          (open-input-string input-string)])
;; 
;;     (check-equal?
;;      (read-cephalapod-arithmetic-part-2 in-port)
;;      "*  1")
;; ;;      #(#\space #\4 #\space #\space))
;; ;;     (check-equal?
;; ;;      (solve-cephalapod-arithmetic-part-2 ceph-arith)
;; ;;      3263827
;;      
;;      
;;   
;;     ))
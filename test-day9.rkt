#lang racket

(require rackunit graph "day9.rkt")


(define
  sample-input
  (format
   (string-append
    "7,1~n"
    "11,1~n"
    "11,7~n"
    "9,7~n"
    "9,5~n"
    "2,5~n"
    "2,3~n"
    "7,3~n"
    )))

(let ([in-port
       (open-input-string sample-input)])
  (check-equal?
   (stream-first
    (read-corner-positions in-port))
   (cons 7 1)))

(let ([in-port
       (open-input-string sample-input)])
  (check-equal?
   (max-rectangle in-port)
   50))

; part 1 full

;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day9-1.txt")])
;;    (check-equal?
;;     (max-rectangle in-port)
;;     --)
;;    )
;;  )

; part 2

(check-exn
 exn:fail?
 (lambda () (green-from-to (cons 1 1) (cons 3 3))))

(check-equal?
 (green-from-to (cons 1 1) (cons 3 1))
 (set (cons 2 1)))

(check-equal?
 (green-from-to (cons 3 5) (cons 3 10))
 (set (cons 3 6) (cons 3 7) (cons 3 8) (cons 3 9)))

; heading up we should be one left of halfway up...
(check-equal?
 (choose-external-point-from-directed-segment (cons 3 3) (cons 3 8))
 (cons 4 5))

(check-equal?
 (choose-external-point-from-directed-segment (cons 3 3) (cons 10 3))
 (cons 6 2))

(check-equal?
 (choose-external-point-from-directed-segment (cons 3 8) (cons 3 3))
 (cons 2 5))

(check-equal?
 (choose-external-point-from-directed-segment (cons 10 3) (cons 3 3))
 (cons 6 4))


(let ([in-port
       (open-input-string sample-input)])
  (let ([corner-positions (read-corner-positions in-port)])
    (check-equal?
     (set-count
      (get-boundary-green-tiles corner-positions))
     22)

    ; we should be able to predict their precise values given what we know
    (check-equal?
     (get-external-points-adjacent-to-boundary corner-positions)
     (set '(12 . 4) '(10 . 8) '(1 . 4) '(4 . 2) '(6 . 2) '(9 . 0) '(5 . 6) '(8 . 6)))))    

(let ([in-port
       (open-input-string sample-input)])
  (let ([corner-positions (read-corner-positions in-port)])
    (check-equal?
     (car
      (car
       (get-rectangles-by-size-descending corner-positions)))
     50)))


(let ([in-port
       (open-input-string sample-input)])
  (check-equal?
   (max-rectangle-part-2 in-port)
   24))

(let ([in-port (open-input-string sample-input)])
  (let ([corner-positions (read-corner-positions in-port)])
    (let ([external-pts (get-external-points-adjacent-to-boundary corner-positions)])
      (for ([pt external-pts])
        (check-true (point-is-external? pt corner-positions)
                    (format "Point ~a should be external" pt))))))

; part 2 full

; curious about how long things take:
; cpu time: 0 real time: 0 gc time: 0
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day9-1.txt")])
;;    (let ([corner-positions (read-corner-positions in-port)])
;;      (let ([red-corners (get-red-corners corner-positions)])
;; 
;;        (check-equal?
;;         (set-count red-corners)
;;         496)
;;        ))))
;; 
;; ; cpu time: 554 real time: 559 gc time: 226
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day9-1.txt")])
;;    (let ([corner-positions (read-corner-positions in-port)])
;;      (let ([boundary-green-tiles (get-boundary-green-tiles corner-positions)])
;; 
;;        (check-equal?
;;         (set-count boundary-green-tiles)
;;         590738)
;;        ))))



        
; cpu time: 11635 real time: 11947 gc time: 2337
;; (time
;;  (let ([in-port
;;         (open-input-file "test-data/input-day9-1.txt")])
;;    (check-equal?
;;     (max-rectangle-part-2 in-port)
;;     17 
;;     )))


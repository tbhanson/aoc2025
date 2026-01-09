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

(let ([in-port
       (open-input-string sample-input)])
  (let ([corner-positions (read-corner-positions in-port)])
    (check-equal?
     (set-count
      (get-boundary-green-tiles corner-positions))
     22)))

(let ([in-port
       (open-input-string sample-input)])
  (let ([corner-positions (read-corner-positions in-port)])
    (check-equal?
     (set-count
      (get-internal-green-tiles corner-positions))
     16)))



  ;; (let ([in-port
  ;;        (open-input-string sample-input)])
  ;;   (check-equal?
  ;;    (max-rectangle-part-2 in-port)
  ;;    24))
  
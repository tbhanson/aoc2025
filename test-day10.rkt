#lang racket

(require rackunit graph "day10.rkt")

(require parser-tools/lex)


(define
  sample-input
  (format
   (string-append
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}~n"
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}~n"
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}~n"
    )))

; test alternative approach claude.ai suggested in response to my question
(let ([in-port
       (open-input-string sample-input)])
  (let ([first-parsed-line
         (stream-first
          (read-manual-line-bits-parsed in-port))])
         
    (check-equal?
     (car first-parsed-line)  ; the pattern
     ".##.")
    
    (check-equal?
     (cadr first-parsed-line)  ; the tuple list
     '((3) (1 3) (2) (2 3) (0 2) (0 1)))
    
    (check-equal?
     (caddr first-parsed-line)  ; the number set
     '(3 5 4 7))

    ))

; test toggle-switches
(check-equal?
 (toggle-switches "." '(0))
 "#")

(check-equal?
 (toggle-switches "..#" '(0 2))
 "#..")

; find-length-of-shortest-toggle-path
(check-equal?
 (find-length-of-shortest-toggle-path "." '((0)))
 0)

(check-equal?
 (find-length-of-shortest-toggle-path "#" '((0)))
 1)

(check-equal?
 (find-length-of-shortest-toggle-path "##" '((0) (1)))
 2)

; check their 3 clues
;; "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}~n"
(check-equal?
 (find-length-of-shortest-toggle-path ".##." '((3) (1 3) (2) (2 3) (0 2) (0 1)))
 2)

;; [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
(check-equal?
 (find-length-of-shortest-toggle-path "...#." '((0 2 3 4) (2 3) (0 4) (0 1 2) (1 2 3 4)))
 3)

;; [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
(check-equal?
 (find-length-of-shortest-toggle-path ".###.#" '((0 1 2 3 4) (0 3 4) (0 1 2 4 5) (1 2)))
 2)

;; line 4 of full input; our method doesn't seem to work here!
;; [.###.] (1,2,4) (0,2,4) (0,1,3,4) (2,4) {23,23,40,3,43}
(check-equal?
 (find-length-of-shortest-toggle-path ".###." '((1 2 4) (0 2 4) (0 1 3 4) (2 4)))
 2)

; total-part1-button-presses
(let ([in-port
       (open-input-string sample-input)])
  (check-equal?
   (total-part1-button-presses in-port)
   7))

; get more systematic and sophisticated
(check-equal?
 (toggle-paths-from "." '((0)) 0)
 (make-immutable-hash (list (cons "." '()))))

(check-equal?
 (toggle-paths-from "." '((0)) 1)
 (make-immutable-hash
  (list
   (cons "." '())
   (cons "#" '((0))))))

(check-equal?
 (toggle-paths-from ".." '((0) (1)) 1)
 (make-immutable-hash
  (list
   (cons ".." '())
   (cons "#." '((0)))
   (cons ".#" '((1))))))

;  (find-length-of-shortest-toggle-path state-to-reach button-choices)
; line 1 full
; [.#...#...#] (3,5,7,8) (0,3,4) (0,1,2,3,4,7,9) (0,1,3,4,6,7,9) (1,4,5,6,8) (0,1,6,9) (0,2,3,4,5,7,8,9) (1,2,5,6,7,9) (0,2,3,5,6,7,8,9) (0,2,3,4,5,7,8)
(check-equal?
 (find-length-of-shortest-toggle-path ".#...#...#" '((3 5 7 8) (0 3 4) (0 1 2 3 4 7 9) (0 1 3 4 6 7 9) (1 4 5 6 8) (0 1 6 9) (0 2 3 4 5 7 8 9) (1 2 5 6 7 9) (0 2 3 5 6 7 8 9) (0 2 3 4 5 7 8)))
 8)


; try first 10 lines 
(time
 (let ([in-port
        (open-input-file "test-data/input-day10-10.txt")])
   (check-equal?
    (total-part1-button-presses in-port)
    30)))

; part 2

(check-equal?
 (distance-to-goal (list 0) (list 0))
 0)

(check-equal?
 (distance-to-goal (list 0) (list 1))
 1)

(check-equal?
 (distance-to-goal (list 0 0) (list 1 2))
 3)

(check-false
 (not-closer-to-goal? (list 1) (list 2) (list 2)))

(check-true
 (not-closer-to-goal? (list 1) (list 1) (list 2)))

(check-true
 (joltage-<=? (list 0) (list 0)))

(check-true
 (joltage-<=? (list 0) (list 1)))

(check-true
 (joltage-<=? (list 0 1 2) (list 0 1 2)))

(check-false
 (joltage-<=? (list 0 1 2) (list 0 1 1)))


(let ([in-port
       (open-input-string sample-input)])

  (check-equal?
   (find-total-part2-button-presses in-port)
   33))

; non-test against small sample
(let ([in-port
       (open-input-string sample-input)])
       ;(open-input-file "test-data/input-day10-10.txt")])
  
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (for ([next-parsed-line stream-of-parsed-lines]
          [line-number (in-naturals 1)])
      (let ([joltage-goal (caddr next-parsed-line)]
            [button-choices (cadr next-parsed-line)])

        (printf " (part2-greedily-get-close-to-but-not-past-goal ~a ~a)~n" joltage-goal button-choices)
        (printf " --> ~a~n"
                (part2-greedily-get-close-to-but-not-past-goal joltage-goal button-choices))))))


; current attempt finishes sample-input, but not first 10 lines of real input 
(let ([in-port
       ;(open-input-string sample-input)])
       (open-input-file "test-data/input-day10-10.txt")])
  
  (let ([stream-of-parsed-lines
         (read-manual-line-bits-parsed in-port)])
    (let ([count-finished
           (for/fold ([count-non-infinite-paths 0])
                     ([next-parsed-line stream-of-parsed-lines]
                      [line-number (in-naturals 1)])
             (let ([joltage-goal (caddr next-parsed-line)]
                   [button-choices (cadr next-parsed-line)])
               
               (let ([this-shortest-path
                      (greedily-find-length-of-shortest-part2-path joltage-goal button-choices)])
                 (printf " ~a: (greedily-find-length-of-shortest-part2-path ~a ~a)~n" line-number joltage-goal button-choices)
                 (printf " --> ~a~n" this-shortest-path)
                 (if (< this-shortest-path +inf.0)
                     (+ count-non-infinite-paths 1)
                     count-non-infinite-paths))))])
      (printf "we computed ~a paths without timing out~n"  count-finished))))
                  



      ;; (time
      ;;  (let ([in-port
      ;;         (open-input-file "test-data/input-day10-10.txt")])
      ;;    (check-equal?
      ;;     (find-total-part2-button-presses in-port)
      ;;     30)))

      
#lang racket

(require rackunit graph "day10.rkt")

(require parser-tools/lex)


;; (let ([in (open-input-string "[.##.] (3)")])
;;   (let loop ()
;;     (let ([tok (manual-lexer in)])
;;       (unless (token-EOF? tok)
;;         (printf "~a~n" tok)
;;         (loop)))))

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


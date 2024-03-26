#lang racket
(require srfi/48)

(provide human-fine)

(define (human-fine bytes)
  (define XB '#(("" . 0) ("kB" . 0) ("MB" . 1) ("GB" . 2) ("TB" . 2) ("PB" . 2)))
  (cond
    [(> bytes 1)
     (let* ([xp (min (exact-floor (log bytes 1024)) (- (vector-length XB) 1))]
            [qt (/ bytes (expt 1024 xp))])
       (cond
         [(= (cdr (vector-ref XB xp)) 0) (~a (~r qt #:precision 0) (car (vector-ref XB xp)))]
         [(= (cdr (vector-ref XB xp)) 1) (format "~0,1F~a" qt (car (vector-ref XB xp)))]
         [(= (cdr (vector-ref XB xp)) 2) (format "~0,2F~a" qt (car (vector-ref XB xp)))]
         [else (error "Fatal error: human-fine(): unexpected decimals count.")]))]
    [(= bytes 0) "0"]
    [(= bytes 1) "1"]
    [else (error (format "Fatal error: human-fine(~a)." bytes))]))

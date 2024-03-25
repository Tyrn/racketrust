#lang racket

(define UNIT-LIST '(("" . 0) ("kB" . 0) ("MB" . 1) ("GB" . 2) ("TB" . 2) ("PB" . 2)))

(define (human-fine bytes)
  (define fb (exact->inexact bytes))
  (cond
    [(> bytes 1)
     (let* ([xp (min (inexact->exact (log fb 1024.0)) (- (length UNIT-LIST) 1))]
            [qt (/ fb (expt 1024.0 xp))])
       (cond
         [(= (cdr (list-ref UNIT-LIST xp)) 0) (format ".0f~a~a" qt (car (list-ref UNIT-LIST xp)))]
         [(= (cdr (list-ref UNIT-LIST xp)) 1) (format ".1f~a~a" qt (car (list-ref UNIT-LIST xp)))]
         [(= (cdr (list-ref UNIT-LIST xp)) 2) (format ".2f~a~a" qt (car (list-ref UNIT-LIST xp)))]
         [else (error "Fatal error: human-fine(): unexpected decimals count.")]))]
    [(= bytes 0) "0"]
    [(= bytes 1) "1"]
    [else (error (format "Fatal error: human-fine(~a)." bytes))]))

;(human-fine 1024) ; example usage

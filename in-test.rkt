#lang racket/base

(require rackunit
         racket/match
         "smallin.rkt")

(define (block-check fn data-block [message ""] [print? #f])
  (for ([i data-block])
    (match-define (list in out) i)
    (check-equal? (apply fn in) out message)
    (when print?
      (printf "~v ~v\n" in out))))

(define initials-block
  (list (list (list "ron reagan-smith") "R.R-S.")
        (list (list "Nell Guinn") "N.G.")
        (list (list "john ronald reuel Tolkien") "J.R.R.T.")))

(block-check initials initials-block "initials test" #t)

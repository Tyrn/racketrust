#lang racket/base

(require rackunit
         racket/match
         "smallin.rkt")

(define (block-check data-block [message ""] [print? #f])
  (for ([i data-block])
    (match-define (list in out) i)
    (check-equal? (initials in) out message)
    (when print? (printf "'~a' '~a'\n" in out))))

(define initials-block
  (list '("ron reagan-smith" "R.R-S.") '("Nell Guinn" "N.G.") '("ben lazar" "B.L.")))

(block-check initials-block "initials test" #t)

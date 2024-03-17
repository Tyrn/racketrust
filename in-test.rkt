#lang racket/base

(require rackunit
         "smallin.rkt")

(define initials-block
  (list '("ron reagan-smith" "R.R-S.") '("Nell Guinn" "N.G.") '("ben lazar" "BL.")))

(check-equal? (initials "ron reagan-smith") "R.R-S." "First try")

(for ([i initials-block])
  (check-equal? (initials (car i)) (car (cdr i)) "initials test")
  (printf "'~a' '~a'\n" (car i) (car (cdr i))))

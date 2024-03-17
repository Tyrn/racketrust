#lang racket/base

(require rackunit
         "smallin.rkt")

(check-equal? (initials "ron reagan-smith") "R.R-S." "First try")

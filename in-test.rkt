#lang racket/base

(require rackunit
         "smallin.rkt")

(check-equal? (authors-join "ron reagan-smith") "R.R-S." "First try")

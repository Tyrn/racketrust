#lang racket/base

(require rackunit
         racket/match
         (prefix-in small: "smallin.rkt"))

(define (block-check fn data-block [message ""] [print? #f])
  (for ([i data-block])
    (match-define (list in out) i)
    (check-equal? (apply fn in) out message)
    (when print?
      (printf "~v ~v\n" in out))))

(define plus-block '(((2 2) 4) ((3 4) 7) ((1 1 1) 3)))
(block-check + plus-block "+" #t)

(define initials-block
  '((("ron reagan-smith") "R.R-S.") (("Nell Guinn") "N.G.")
                                    (("john ronald reuel Tolkien") "J.R.R.T.")))
(block-check small:initials initials-block "initials" #t)

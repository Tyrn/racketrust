#lang racket/base
(require racket/string)

(provide initials)

(define (initial-create str)
  (string-upcase (substring str 0 1)))

(define (by-names-split author)
  (regexp-split #rx"[ .]+" author))
(define (by-barrels-split barrels)
  (string-split barrels "-"))
(define (by-authors-split authors)
  (string-split authors ","))

(define (initials-join author)
  (string-join (map (λ (name) (initial-create name)) (by-names-split author)) "."))
(define (barrels-join barrels)
  (string-append (string-join (map (λ (barrel) (initials-join barrel)) (by-barrels-split barrels))
                              "-")
                 "."))

(define (initials authors)
  (string-join (map (λ (author) (barrels-join author)) (by-authors-split authors)) ","))

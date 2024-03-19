#lang racket/base
(require racket/string)

(provide initials)

(define (initial-create str)
  (string-upcase (substring str 0 1)))

(define (into-names-split barrel)
  (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))

(define (into-barrels-split author)
  (filter (λ (barrel) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
          (string-split author "-")))

(define (into-authors-split coauthors)
  (filter (λ (author) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
          (string-split coauthors ",")))

(define (names-split-n-join barrel)
  (string-join (map (λ (name) (initial-create name)) (into-names-split barrel)) "."))

(define (barrels-split-n-join barrels)
  (string-append (string-join (map (λ (barrel) (names-split-n-join barrel)) (into-barrels-split barrels))
                              "-")
                 "."))

(define (initials coauthors)
  (string-join (map (λ (author) (barrels-split-n-join author)) (into-authors-split coauthors)) ","))

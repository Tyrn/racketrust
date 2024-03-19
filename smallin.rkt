#lang typed/racket
(require racket/string)

(provide initials)

(: initial-create (-> String String))
(define (initial-create str)
  (string-upcase (substring str 0 1)))

(: into-names-split (-> String (Listof String)))
(define (into-names-split barrel)
  (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))

(: into-barrels-split (-> String (Listof String)))
(define (into-barrels-split author)
  (filter (λ ([barrel : String]) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
          (string-split author "-")))

(: into-authors-split (-> String (Listof String)))
(define (into-authors-split coauthors)
  (filter (λ ([author : String]) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
          (string-split coauthors ",")))

(: names-split-n-join (-> String String))
(define (names-split-n-join barrel)
  (string-join (map (λ (name) (initial-create name)) (into-names-split barrel)) "."))

(: barrels-split-n-join (-> String String))
(define (barrels-split-n-join barrels)
  (string-append (string-join (map (λ (barrel) (names-split-n-join barrel)) (into-barrels-split barrels))
                              "-")
                 "."))

(: initials (-> String String))
(define (initials coauthors)
  (string-join (map (λ (author) (barrels-split-n-join author)) (into-authors-split coauthors)) ","))

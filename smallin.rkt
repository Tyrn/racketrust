#lang typed/racket
(require racket/string)

(provide initials)

(: initial-create (-> String String))
(define (initial-create str)
  (string-upcase (substring str 0 1)))

(: into-names-split (-> String (Listof String)))
(define (into-names-split author)
  (regexp-split #px"[\\s.]+" author))

(: into-barrels-split (-> String (Listof String)))
(define (into-barrels-split barrels)
  (filter (λ ([barrel : String]) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
          (string-split barrels "-")))

(: into-authors-split (-> String (Listof String)))
(define (into-authors-split authors)
  (filter (λ ([author : String]) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
          (string-split authors ",")))

(: initials-join (-> String String))
(define (initials-join author)
  (string-join (map (λ (name) (initial-create name)) (into-names-split author)) "."))

(: barrels-join (-> String String))
(define (barrels-join barrels)
  (string-append (string-join (map (λ (barrel) (initials-join barrel)) (into-barrels-split barrels))
                              "-")
                 "."))

(: initials (-> String String))
(define (initials authors)
  (string-join (map (λ (author) (barrels-join author)) (into-authors-split authors)) ","))

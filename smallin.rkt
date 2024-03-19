#lang typed/racket
(require racket/string)

(provide initials)

(: initial-create (-> String String))
(define (initial-create str)
  (string-upcase (substring str 0 1)))

(: by-names-split (-> String (Listof String)))
(define (by-names-split author)
  (regexp-split #px"[\\s.]+" author))

(: by-barrels-split (-> String (Listof String)))
(define (by-barrels-split barrels)
  (string-split barrels "-"))

(: by-authors-split (-> String (Listof String)))
(define (by-authors-split authors)
  (string-split authors ","))

(: initials-join (-> String String))
(define (initials-join author)
  (string-join (map (λ (name) (initial-create name)) (by-names-split author)) "."))

(: barrels-join (-> String String))
(define (barrels-join barrels)
  (string-append (string-join (map (λ (barrel) (initials-join barrel)) (by-barrels-split barrels))
                              "-")
                 "."))

(: initials (-> String String))
(define (initials authors)
  (string-join (map (λ (author) (barrels-join author)) (by-authors-split authors)) ","))

#lang racket/base
(require racket/string)

(provide initials
         initials-1
         initials-2
         initials-3)

(define (initial-create str)
  (string-upcase (substring str 0 1)))

(define (initials coauthors)
  ;; Returns the string without double-quoted (") substrings; drops an odd
  ;; double-quote too, if any.
  (define (nicknames-drop coauthors)
    (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ") "\"" " "))

  ;; At least one character besides trash (spaces, dots, and dashes).
  (define (valid-author? author)
    (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))

  ;; At least one character besides trash (spaces and dots).
  (define (valid-barrel? barrel)
    (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))

  ;; Splits a string on spaces and/or dots.
  (define (into-valid-names-split barrel)
    (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))

  (string-join
   (map (λ (author)
          (string-append (string-join (map (λ (barrel)
                                             (string-join (map (λ (name) (initial-create name))
                                                               (into-valid-names-split barrel))
                                                          "."))
                                           (filter (λ (barrel) (valid-barrel? barrel))
                                                   (string-split author "-")))
                                      "-")
                         "."))
        (filter (λ (author) (valid-author? author)) (string-split (nicknames-drop coauthors) ",")))
   ","))

(define (initials-1 coauthors)
  (define (nicknames-drop coauthors)
    (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ") "\"" " "))

  (define (into-authors-split coauthors)
    (filter (λ (author) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
            (string-split coauthors ",")))

  (define (into-barrels-split-n-join author)
    (define (into-names-split-n-join barrel)
      (define (into-names-split barrel)
        (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))

      (string-join (map (λ (name) (initial-create name)) (into-names-split barrel)) "."))

    (define (into-barrels-split author)
      (filter (λ (barrel) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
              (string-split author "-")))

    (string-append
     (string-join (map (λ (barrel) (into-names-split-n-join barrel)) (into-barrels-split author)) "-")
     "."))

  (string-join (map (λ (author) (into-barrels-split-n-join author))
                    (into-authors-split (nicknames-drop coauthors)))
               ","))

(define (initials-2 coauthors)
  (string-join
   (map (λ (author)
          (string-append
           (string-join
            (map (λ (barrel)
                   (string-join (map (λ (name) (initial-create name))
                                     (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))
                                "."))
                 (filter (λ (barrel) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
                         (string-split author "-")))
            "-")
           "."))
        (filter (λ (author) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
                (string-split
                 (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ") "\"" " ")
                 ",")))
   ","))

#;{String -> String}
(define (initials-3 coauthors)
  (let* ([s (nicknames-drop coauthors)]
         [s (string-split s ",")]
         [s (filter valid-author? s)]
         [s (map author->initialed-author s)]
         [s (string-join s ",")])
    s))

;; -----------------------------------------------------------------------------
#;{String -> String}
(define (nicknames-drop coauthors)
  (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ") "\"" " "))

;; -----------------------------------------------------------------------------
#;{String -> Boolean}
(define (valid-author? author)
  (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))

;; -----------------------------------------------------------------------------
;; convert author to initials, if it consists of valid barrels

#;{String -> String}
(define (author->initialed-author author)
  (let* ([s (string-split author "-")]
         [s (filter valid-barrel? s)]
         [s (map barrel->initialed-names s)]
         [s (string-join s "-")]
         [s (string-append s ".")])
    s))

#;{String -> Boolean}
(define (valid-barrel? barrel)
  (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))

#;{String -> String}
(define (barrel->initialed-names barrel)
  (let* ([s (into-valid-names-split barrel)] [s (map initial-create s)] [s (string-join s ".")]) s))

#;{String -> [Listof Boolean]}
(define (into-valid-names-split barrel)
  (filter non-empty-string? (regexp-split #px"[\\s.]+" barrel)))

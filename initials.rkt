#lang racket/base
(require racket/string)

(provide initials
         initials-1
         initials-2)

(define (initial-create str)
  (string-upcase (substring str 0 1)))

(define (initials coauthors)
  (define (nicknames-drop coauthors)
    (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ") "\"" " "))

  (define (valid-author? author)
    (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))

  (define (valid-barrel? barrel)
    (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))

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

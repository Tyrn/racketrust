#lang racket/base
(require racket/string
         racket/list)

(provide initials
         initials-1
         initials-2
         initials-3)

(define (name-prefix str)
  (define char* (string->list str))
  (let while ([char* (rest char*)] [result (list (first char*))])
    (cond
      [(empty? char*) (list->string (reverse result))]
      [else
       (define c (first char*))
       (cond
         [(char-upper-case? c) (list->string (reverse (cons c result)))]
         [(char-lower-case? c) (while (rest char*) (cons c result))]
         [else (while (rest char*) result)])])))

(define (initial-create str)
  (let ([o-neal (regexp-match #px".*?'([^']{1})" str)])
    (if (not (equal? o-neal #f))
        (car o-neal)
        (let ([stub (name-prefix str)])
          (if (equal? stub (string-replace str "'" "")) (string-upcase (substring str 0 1)) stub)))))

(define (initials coauthors)
  (define (author->initialed-author author)
    (define (barrel->initialed-names barrel)
      (let* ([s (filter non-empty-string?
                        (regexp-split #px"[\\s.]+" barrel))] ;; Split into valid names.
             [s (map initial-create s)] ;; Convert names to initials.
             [s (string-join s ".")]) ;; The last initial without dot cover.
        s))

    (let* ([s (string-split author "-")]
           [s (filter (λ (barrel) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
                      s)] ;; Valid barrel may pass.
           [s (map barrel->initialed-names s)]
           [s (string-join s "-")]
           [s (string-append s ".")]) ;; On author the final dot.
      s))

  (let* ([s (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ")
                            "\""
                            " ")] ;; Drop double-quoted (") substrings. Odd double-quote, too.
         [s (string-split s ",")]
         [s (filter (λ (author) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
                    s)] ;; Valid author may pass.
         [s (map author->initialed-author s)]
         [s (string-join s ",")])
    s))

(define (initials-1 coauthors)
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

(define (initials-2 coauthors)
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

(define (initials-3 coauthors)
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

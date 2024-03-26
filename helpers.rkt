#lang racket
(require racket/string
         racket/list
         srfi/48)

(provide human-fine
         initials)

(define (human-fine bytes)
  (define XB '#(("" . 0) ("kB" . 0) ("MB" . 1) ("GB" . 2) ("TB" . 2) ("PB" . 2)))
  (cond
    [(> bytes 1)
     (let* ([xp (min (exact-floor (log bytes 1024)) (- (vector-length XB) 1))]
            [qt (/ bytes (expt 1024 xp))])
       (cond
         [(= (cdr (vector-ref XB xp)) 0) (~a (~r qt #:precision 0) (car (vector-ref XB xp)))]
         [(= (cdr (vector-ref XB xp)) 1) (format "~0,1F~a" qt (car (vector-ref XB xp)))]
         [(= (cdr (vector-ref XB xp)) 2) (format "~0,2F~a" qt (car (vector-ref XB xp)))]
         [else (error "Fatal error: human-fine(): unexpected decimals count.")]))]
    [(= bytes 0) "0"]
    [(= bytes 1) "1"]
    [else (error (format "Fatal error: human-fine(~a)." bytes))]))

(define nobiliary-particles
  '("von" "фон"
          "van"
          "ван"
          "der"
          "дер"
          "til"
          "тиль"
          "zu"
          "цу"
          "zum"
          "цум"
          "zur"
          "цур"
          "af"
          "аф"
          "of"
          "из"
          "da"
          "да"
          "de"
          "де"
          "des"
          "дез"
          "del"
          "дель"
          "di"
          "ди"
          "dos"
          "душ"
          "дос"
          "du"
          "дю"
          "la"
          "ла"
          "ля"
          "le"
          "ле"
          "haut"
          "от"
          "the"))

(define (initial-create name)
  (define full-nior
    (case name
      [("Старший") "Ст"]
      [("Младший") "Мл"]
      [else #f]))
  (define nior (member name '("Ст" "ст" "Sr" "Мл" "мл" "Jr")))
  (define sax-gen (regexp-match #px".*?'([^']{1})" name)) ; Takes care of O'Connor.
  (define (name-prefix s) ; Takes care of DiCaprio.
    (car (regexp-match #px"^.\\p{Ll}*\\p{Lu}?" s)))
  (cond
    [sax-gen (first sax-gen)]
    [(member name nobiliary-particles) (substring name 0 1)]
    [full-nior full-nior]
    [nior (first nior)]
    [else
     (define stub (name-prefix name))
     (cond
       [(equal? stub (string-replace name "'" "")) (string-upcase (substring name 0 1))]
       [else stub])]))

(define (initials coauthors)
  (define (author->initialed-author author)
    (define (barrel->initialed-names barrel)
      (let* ([s (filter non-empty-string?
                        (regexp-split #px"[\\s.]+" barrel))] ; Split into valid names.
             [s (map initial-create s)] ; Convert names to initials.
             [s (string-join s ".")]) ; The last initial remains without dot cover.
        s))

    (let* ([s (string-split author "-")]
           [s (filter (λ (barrel) (non-empty-string? (regexp-replace* #px"[\\s.]+" barrel "")))
                      s)] ; Valid barrel may pass.
           [s (map barrel->initialed-names s)]
           [s (string-join s "-")]
           [s (string-append s ".")]) ; The final barrel gets the final dot.
      s))

  (let* ([s (string-replace (regexp-replace* #rx"\"(?:\\.|[^\"\\])*\"" coauthors " ")
                            "\""
                            " ")] ; Drop double-quoted (") substrings. Odd double-quote, too.
         [s (string-split s ",")]
         [s (filter (λ (author) (non-empty-string? (regexp-replace* #px"[\\s.\\-]+" author "")))
                    s)] ; Valid author may pass.
         [s (map author->initialed-author s)]
         [s (string-join s ",")])
    s))

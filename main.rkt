#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To create an executable
;;   $ raco exe -o hello hello.rkt
;;
;; see https://docs.racket-lang.org/raco/exe.html
;;
;; To share stand-alone executables:
;;   $ raco distribute <directory> executable ...
;;
;; e.g
;;   $ raco distribute greetings hello.exe
;;
;; creates a directory "greetings" (if the directory doesn’t exist already),
;; and it copies the executables "hello.exe" and "goodbye.exe" into "greetings".
;;
;; see https://docs.racket-lang.org/raco/exe-dist.html
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here
(require racket/cmdline
         racket/port
         racket/string
         racket/list
         racket/set)
(define who (box "world"))
(define (pipediput)
  (for ([line (port->lines)])
    (printf "hello ~a~n" line)))
(command-line #:program "Greeter"
              #:once-any [("-n" "--name") name "Who to say hello to" (set-box! who name)]
              [("-p" "--pipe") "greet piped list" (pipediput)]
              #:args ()
              (printf "hello ~a~n" (unbox who)))

(define (string-blank? str)
  regexp-match-exact?
  #px"\\s*"
  str)

(define (form-initial name)
  "Makes an initial out of name,
  handling special cases like von, Mc, O', etc."
  (let ([cut (string-split name #"'")])
    (cond
      ;; Deal with O'Connor and d'Artagnan.
      [(and (> (length cut) 1) (not (non-empty-string? (list-ref cut 1))))
       (cond
         [(and (char-lower-case? (string-ref (list-ref cut 1) 0)))
          (string-upcase (string (string-ref (list-ref cut 0) 0)))]
         [else (string-append (list-ref cut 0) "'" (list-ref cut 1))])]
      ;; Deal with Leonardo DiCaprio and Jackie McGee.
      [(and (> (length name) 1) (ormap char-upper-case? (rest name)))
       (let loop ([tail (rest name)] [prefix (first name)])
         (if (char-upper-case? (first tail))
             (string-append prefix (string (first tail)))
             (loop (rest tail) (string-append prefix (string (first tail))))))]
      [(set-member? #{"von"
                      "фон"
                      "van"
                      "ван"
                      "der"
                      "дер"
                      "til"
                      "тиль"
                      "zu"
                      "цу"
                      "af"
                      "аф"
                      "of"
                      "из"
                      "de"
                      "де"
                      "des"
                      "дез"
                      "del"
                      "дель"
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
                      "от"}
                    name)
       (string (first name))]
      [else (string-upcase (string (first name)))])))

(define (initials authors)
  "Reduces comma separated list of
  authors to initials."
  (string-join
   (map
    (lambda (author)
      (let* ([replaced
              (regexp-replace* #px"\"(?:\\.|\\[^\"\\])*\"" authors " ")] ;; Remove quoted substrings.
             [odd-quotes-replaced (regexp-replace* #\" replaced " ")] ;; Remove odd quotes.
             [split-authors (string-split odd-quotes-replaced #",")]
             [filtered-authors (filter (lambda (author)
                                         (not (string-blank? (regexp-replace* #px"[-.]+" author ""))))
                                       split-authors)]
             [formatted-authors
              (map (lambda (author)
                     (let* ([split-barrels (string-split author #"-")]
                            [filtered-barrels
                             (filter (lambda (barrel)
                                       (not (string-blank? (regexp-replace* #px"[.]+" + barrel ""))))
                                     split-barrels)]
                            [raw-initials (map form-initial filtered-barrels)]
                            [joined-barrels (string-join raw-initials ".")])
                       joined-barrels))
                   filtered-authors)])
        (string-join formatted-authors "-")))
    (string-split authors #","))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (check-equal? (unbox who) "world"))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
  (define who (box "world")))

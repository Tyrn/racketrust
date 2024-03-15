#!/usr/bin/env racket
#lang racket

(require racket/path
         racket/stream
         racket/file)

; Function to list all files and directories in a directory
(define (children parent)
  (define-values (all-items) (directory-list parent #:build? #t))
  (let-values ([(dirs files) (partition directory-exists? all-items)])
    (values dirs files)))

;; Function to traverse directories and produce a flat and lazy stream of files
(define (traverse parent)
  (define-values (dirs files) (children parent))
  (stream-append (apply stream-append
                        (for/list ([dir (in-list dirs)])
                          (stream-lazy (traverse dir))))
                 files))

(define reds (stream-cons "red" reds))

; Main function to traverse directories and print matching files
(define (traverse-and-print)
  (define-values (dirs files) (children "."))
  (displayln (format "dirs: ~a" dirs))
  (displayln (format "files: ~a" files))

  (for ([item (in-stream (traverse "."))] [idx (in-naturals 1)])
    (printf "~a ~a\n" idx item)))

; Run the main function
(traverse-and-print)

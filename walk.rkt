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
(define (traverse-one parent)
  (define-values (dirs files) (children parent))
  (stream-append (apply stream-append
                        (for/list ([dir (in-list dirs)])
                          (stream-lazy (traverse-one dir))))
                 files))

;; Function to traverse directories backwards and produce a flat and lazy stream of files
(define (t-raverse-one parent)
  (define-values (dirs files) (children parent))
  (stream-append files
                 (apply stream-append
                        (for/list ([dir (in-list dirs)])
                          (stream-lazy (t-raverse-one dir))))))

;; Function to traverse directories and produce a flat and lazy stream of files
(define (traverse parent)
  (stream-lazy (let-values ([(dirs files) (children parent)])
                 (stream-append (apply stream-append (map traverse dirs)) files))))

;; Function to traverse directories backwards and produce a flat and lazy stream of files
(define (t-raverse parent)
  (stream-lazy (let-values ([(dirs files) (children parent)])
                 (stream-append files (apply stream-append (map t-raverse dirs))))))

(define reds (stream-cons "red" reds))

; Main function to traverse directories and print matching files
(define (traverse-and-print)
  (define-values (dirs files) (children "."))
  (displayln (format "dirs: ~a" dirs))
  (displayln (format "files: ~a" files))

  (for ([item (in-stream (t-raverse "."))] [idx (in-naturals 1)])
    (printf "~a ~a\n" idx item)))

; Run the main function
(traverse-and-print)

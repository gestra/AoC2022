#lang racket

(define input (file->lines "../inputs/day04.txt"))
(define test (list "2-4,6-8" "2-3,4-5" "5-7,7-9" "2-8,3-7" "6-6,4-6" "2-6,4-8"))

(define (fully-contained? pair)
  (let ([re (regexp-match #rx"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" pair)])
    (let ([e1-start (string->number (list-ref re 1))]
          [e1-end (string->number (list-ref re 2))]
          [e2-start (string->number (list-ref re 3))]
          [e2-end (string->number (list-ref re 4))])
      (if (or (and (<= e1-start e2-start) (>= e1-end e2-end))
              (and (<= e2-start e1-start) (>= e2-end e1-end)))
          #t
          #f))))

(define (any-overlap? pair)
  (let ([re (regexp-match #rx"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" pair)])
    (let ([e1-start (string->number (list-ref re 1))]
          [e1-end (string->number (list-ref re 2))]
          [e2-start (string->number (list-ref re 3))]
          [e2-end (string->number (list-ref re 4))])
      (if (or (and (>= e1-end e2-start) (<= e1-end e2-end))
              (and (>= e2-end e1-start) (<= e2-end e1-end)))
          #t
          #f))))

(printf "Part 1: ~a\n" (count fully-contained? input))
(printf "Part 2: ~a\n" (count any-overlap? input))
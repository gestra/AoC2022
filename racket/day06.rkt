#lang racket

(define input (string->list (file->string "../inputs/day06.txt")))
(define test (string->list "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))

(define (find-marker lst num-chars)
  (define (is-marker? lst)
    (cond
      [(< (length lst) num-chars) #f]
      [(not (for*/or ([i (range 0 num-chars)]
                      [j (range (+ 1 i) num-chars)])
              (char=? (list-ref lst i) (list-ref lst j))))
       #t]
      [else #f]))
  
  (for/first ([i (in-range 0 (length lst))]
              #:when (is-marker? (list-tail lst i)))
    (+ num-chars i)))

(printf "Part 1: ~a\n" (find-marker input 4))
(printf "Part 1: ~a\n" (find-marker input 14))
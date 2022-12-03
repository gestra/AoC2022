#lang racket

(define input (map string->list (file->lines "../inputs/day03.txt")))
;(define input (map string->list (list "vJrwpWtwJgWrhcsFMMfFFhFp"
;                                      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
;                                      "PmmdzqPrVvPwwTWBwg"
;                                      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
;                                      "ttgJtRGJQctTZtZT"
;                                      "CrZsJsPPZsGzwwsLwLmpwMDw")))

(define (common-in-both-halves rucksack)
  (let ([first-half (take rucksack (/ (length rucksack) 2))]
        [second-half (list-tail rucksack (/ (length rucksack) 2))])
    (for*/first ([f first-half]
                 [s second-half]
                 #:when (char=? f s))
      f)))

(define (char-value c)
  (cond
    [(char-lower-case? c) (- (char->integer c) 96)]
    [else (- (char->integer c) 38)]))

(define (solve-part-1 lst)
  (cond
    [(empty? lst) 0]
    [else (+
           (char-value (common-in-both-halves (first lst)))
           (solve-part-1 (rest lst)))]))

(define (common-in-three-rucksacks lst)
  (for*/first ([f (first lst)]
               [s (first (rest lst))]
               [t (first (rest (rest lst)))]
               #:when (char=? f s t))
    f))

(define (solve-part-2 lst)
  (cond
    [(empty? lst) 0]
    [else (+
           (char-value (common-in-three-rucksacks (take lst 3)))
           (solve-part-2 (list-tail lst 3)))]))

(printf "Part 1: ~a\n" (solve-part-1 input))
(printf "Part 2: ~a\n" (solve-part-2 input))
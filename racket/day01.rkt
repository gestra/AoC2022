#lang racket

(define input (map string->number (file->lines "../inputs/day01.txt")))
;(define input (list 1000 2000 3000 #f 4000 #f 5000 6000 #f 7000 8000 9000 #f 10000))

(define (total-calories lst)
  (define (iter lst acc)
    (cond
      [(empty? lst) empty]
      [(empty? (rest lst)) (list (+ acc (first lst)))]
      [(false? (first lst)) (cons acc (iter (rest lst) 0))]
      [else (iter (rest lst) (+ acc (first lst)))]))
  (iter lst 0))

(define sorted (sort (total-calories input) >))

(printf "Part 1: ~a\n" (first sorted))
(printf "Part 2: ~a\n" (+ (first sorted) (first (rest sorted)) (first (rest (rest sorted)))))
#lang racket

(define input (file->lines "../inputs/day02.txt"))
;(define input (list "A Y" "B X" "C Z"))

(define (round-score round)
  (let ([opponents-hand (string-ref round 0)]
        [my-hand (string-ref round 2)])
    (let ([score (match opponents-hand
                   [#\A (match my-hand
                          [#\X 3]
                          [#\Y 6]
                          [#\Z 0])]
                   [#\B (match my-hand
                          [#\X 0]
                          [#\Y 3]
                          [#\Z 6])]
                   [#\C (match my-hand
                          [#\X 6]
                          [#\Y 0]
                          [#\Z 3])])])
      (+ score (match my-hand
                 [#\X 1]
                 [#\Y 2]
                 [#\Z 3])))))

(define (decrypt-hand hand)
  (let ([opponents-hand (string-ref hand 0)]
        [result (string-ref hand 2)])
    (let ([my-hand (match opponents-hand
      [#\A (match result
             [#\X #\Z]
             [#\Y #\X]
             [#\Z #\Y])]
      [#\B (match result
             [#\X #\X]
             [#\Y #\Y]
             [#\Z #\Z])]
      [#\C (match result
             [#\X #\Y]
             [#\Y #\Z]
             [#\Z #\X])])])
      (string opponents-hand #\space my-hand))))

(define (calculate-score-p1 guide)
  (cond
    [(empty? guide) 0]
    [else (+ (round-score (first guide))
             (calculate-score-p1 (rest guide)))]))

(define (calculate-score-p2 guide)
  (cond
    [(empty? guide) 0]
    [else (+ (round-score (decrypt-hand (first guide)))
             (calculate-score-p2 (rest guide)))]))

(printf "Part 1: ~a\n" (calculate-score-p1 input))
(printf "Part 2: ~a\n" (calculate-score-p2 input))
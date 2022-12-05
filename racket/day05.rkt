#lang racket

(define input (file->lines "../inputs/day05.txt"))
(define test (list
              "    [D]    "
              "[N] [C]    "
              "[Z] [M] [P]"
              " 1   2   3"
              ""
              "move 1 from 2 to 1"
              "move 3 from 1 to 3"
              "move 2 from 2 to 1"
              "move 1 from 1 to 2"))

(define (procedures lst)
  (list-tail lst (+ 1 (index-of lst ""))))

(define (starting-pos lst)
  (map string->list (take lst (- (index-of lst "") 1))))

(define (parse-starting-pos positions)
  (list->vector
   (build-list
    (ceiling (/ (apply max (map length  positions)) 4))
    (lambda (x)
      (let ([i (+ 1 (* 4 x))])
        (list->vector
         (for/list ([row positions]
                    #:when (and (< i (length row)) (char-alphabetic? (list-ref row i))))
           (list-ref row i))))))))

(define (apply-procedure stacks procedure new-model?)
  (let* ([re (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" procedure)]
         [quantity (string->number(list-ref re 1))]
         [from-i (- (string->number(list-ref re 2)) 1)]
         [to-i (- (string->number(list-ref re 3)) 1)])
    (define (vec-rev v)
      (build-vector (vector-length v) (lambda (x) (vector-ref v (- (- (vector-length v) x) 1)))))
    (vector-set!
     stacks
     to-i
     (if new-model?
         (vector-append
          (vector-take (vector-ref stacks from-i) quantity)
          (vector-ref stacks to-i))
         (vector-append
          (vec-rev (vector-take (vector-ref stacks from-i) quantity))
          (vector-ref stacks to-i))))
    (vector-set!
     stacks
     from-i
     (vector-drop (vector-ref stacks from-i) quantity))
    stacks))

(define (apply-all-procedures stacks procedures new-model?)
  (for ([p procedures])
    (set! stacks (apply-procedure stacks p new-model?)))
  stacks)

(define (solve in new-model?)
  (list->string
   (vector->list
    (vector-map (lambda (x) (vector-ref x 0))
                (apply-all-procedures
                 (parse-starting-pos (starting-pos in))
                 (procedures in)
                 new-model?)))))

(printf "Part 1: ~a\n" (solve input #f))
(printf "Part 2: ~a\n" (solve input #t))
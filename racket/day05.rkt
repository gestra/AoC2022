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
  (build-list
   (ceiling (/ (apply max (map length  positions)) 4))
   (lambda (x)
     (let ([i (+ 1 (* 4 x))])
       (for/list ([row positions]
                  #:when (and (< i (length row)) (char-alphabetic? (list-ref row i))))
         (list-ref row i))))))

(define (apply-procedure stacks procedure new-model?)
  (let* ([re (regexp-match #rx"move ([0-9]+) from ([0-9]+) to ([0-9]+)" procedure)]
         [quantity (string->number(list-ref re 1))]
         [from-i (- (string->number(list-ref re 2)) 1)]
         [to-i (- (string->number(list-ref re 3)) 1)])
    (for/list ([i (in-range (length stacks))])
      (let ([stack (list-ref stacks i)])
        (cond
          [(= i from-i) (list-tail stack quantity)]
          [(= i to-i) (if new-model?
                          (append (take (list-ref stacks from-i) quantity) stack)
                          (append (reverse (take (list-ref stacks from-i) quantity)) stack))]
          [else stack])))))
                      
(define (apply-all-procedures stacks procedures new-model?)
  (cond
    [(empty? procedures) stacks]
    [else
     (apply-all-procedures
      (apply-procedure stacks (first procedures) new-model?)
      (rest procedures)
      new-model?)]))

(define (solve in new-model?)
  (list->string
   (map (lambda (x) (first x))
        (apply-all-procedures
         (parse-starting-pos (starting-pos in))
         (procedures in)
         new-model?))))

(printf "Part 1: ~a\n" (solve input #f))
(printf "Part 2: ~a\n" (solve input #t))
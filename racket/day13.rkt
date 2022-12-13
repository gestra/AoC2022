#lang racket

(define input (file->lines "../inputs/day13.txt"))
(define test (list
              "[1,1,3,1,1]"
              "[1,1,5,1,1]"
              ""
              "[[1],[2,3,4]]"
              "[[1],4]"
              ""
              "[9]"
              "[[8,7,6]]"
              ""
              "[[4,4],4,4]"
              "[[4,4],4,4,4]"
              ""
              "[7,7,7,7]"
              "[7,7,7]"
              ""
              "[]"
              "[3]"
              ""
              "[[[]]]"
              "[[]]"
              ""
              "[1,[2,[3,[4,[5,6,7]]]],8,9]"
              "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

(define ns (make-base-namespace))
(define (parse-input lst)
  (define (parse-line line)
    (let ([s (string-replace (string-replace (string-replace line "[" "(list ") "]" ")") "," " ")])
      (eval (call-with-input-string s read) ns)))
  (cond
    [(empty? lst) empty]
    [(= (length lst) 2) (list (cons (parse-line (first lst)) (parse-line (second lst))))]
    [else (cons (cons (parse-line (first lst)) (parse-line (second lst))) (parse-input (list-tail lst 3)))]))

(define (right-order? left right)
  (cond
    [(empty? left) #t]
    [(empty? right) #f]
    [(and (number? (first left)) (number? (first right)))
     (cond
       [(< (first left) (first right)) #t]
       [(> (first left) (first right)) #f]
       [else (right-order? (rest left) (rest right))])]
    [(and (list? (first left)) (list? (first right)))
     (cond
       [(equal? (first left) (first right)) (right-order? (rest left) (rest right))]
       [else (right-order? (first left) (first right))])]
    [else
     (cond
       [(number? (first left)) (right-order? (cons (list (first left)) (rest left)) right)]
       [else (right-order? left (cons (list (first right)) (rest right)))])]))

(define (solve-1 pairs)
  (let ([orders (map (lambda (x) (right-order? (car x) (cdr x))) pairs)])
    (for/sum ([i (in-range (length orders))]
              #:when (list-ref orders i))
      (add1 i))))

(define (solve-2 pairs)
  (define (collect-individual p)
    (cond
      [(empty? p) empty]
      [else (cons (car (first p)) (cons (cdr (first p)) (collect-individual (rest p))))]))
  (let* ([div1 (list 2)]
         [div2  (list 6)]
         [sorted (sort (cons div2 (cons div1 (collect-individual pairs))) right-order?)])
    (* (add1 (index-of sorted div1)) (add1 (index-of sorted div2)))))

(define pairs (parse-input input))
(printf "Part 1: ~a\n" (solve-1 pairs))
(printf "Part 2: ~a\n" (solve-2 pairs))
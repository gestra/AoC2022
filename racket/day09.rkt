#lang racket

(define input (file->lines "../inputs/day09.txt"))
(define test (list "R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"))
(define large-test (list "R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"))

(define (individual-steps lst)
  (define (steps line)
    (let ([dir (string-ref line 0)]
          [num (string->number (list-ref (regexp-match #rx"([0-9]+)" line) 1))])
      (for/list ([i (in-range num)])
        dir)))
  (cond
    [(empty? lst) empty]
    [else
     (append (steps (first lst)) (individual-steps (rest lst)))]))

(define (apply-move dir pos)
  (match dir
    [#\U (cons (car pos) (add1 (cdr pos)))]
    [#\D (cons (car pos) (sub1 (cdr pos)))]
    [#\L (cons (sub1 (car pos)) (cdr pos))]
    [#\R (cons (add1 (car pos)) (cdr pos))]))

(define (move-tail head-pos tail-pos)
  (cond
    ; Same row
    [(= (cdr head-pos) (cdr tail-pos))
     (cond
       ; Move right
       [(<= 2 (- (car head-pos) (car tail-pos)))
        (cons (add1 (car tail-pos)) (cdr tail-pos))]
       ; Move left
       [(>= -2 (- (car head-pos) (car tail-pos)))
        (cons (sub1 (car tail-pos)) (cdr tail-pos))]
       [else tail-pos])]
    ; Same column
    [(= (car head-pos) (car tail-pos))
     (cond
       ; Up
       [(<= 2 (- (cdr head-pos) (cdr tail-pos)))
        (cons (car tail-pos) (add1 (cdr tail-pos)))]
       ; Down
       [(>= -2 (- (cdr head-pos) (cdr tail-pos)))
        (cons (car tail-pos) (sub1 (cdr tail-pos)))]
       [else tail-pos])]
    ; Up-right
    [(and (> (car head-pos) (car tail-pos)) (> (cdr head-pos) (cdr tail-pos)))
     (if (or (<= 2 (- (car head-pos) (car tail-pos))) (<= 2 (- (cdr head-pos) (cdr tail-pos))))
         (cons (add1 (car tail-pos)) (add1 (cdr tail-pos)))
         tail-pos)]
    ; Up-left
    [(and (< (car head-pos) (car tail-pos)) (> (cdr head-pos) (cdr tail-pos)))
     (if (or (>= -2 (- (car head-pos) (car tail-pos))) (<= 2 (- (cdr head-pos) (cdr tail-pos))))
         (cons (sub1 (car tail-pos)) (add1 (cdr tail-pos)))
         tail-pos)]
    ; Down-right
    [(and (> (car head-pos) (car tail-pos)) (< (cdr head-pos) (cdr tail-pos)))
     (if (or (<= 2 (- (car head-pos) (car tail-pos))) (>= -2 (- (cdr head-pos) (cdr tail-pos))))
         (cons (add1 (car tail-pos)) (sub1 (cdr tail-pos)))
         tail-pos)]
    ; Down-left
    [(and (< (car head-pos) (car tail-pos)) (< (cdr head-pos) (cdr tail-pos)))
     (if (or (>= -2 (- (car head-pos) (car tail-pos))) (>= -2 (- (cdr head-pos) (cdr tail-pos))))
         (cons (sub1 (car tail-pos)) (sub1 (cdr tail-pos)))
         tail-pos)]))

(define (last-knot-history lst num-knots)
  (define (move-knots knots)
    (cond
      [(<= (length knots) 1) knots]
      [else (cons
             (first knots)
             (move-knots
              (cons (move-tail (first knots) (first (rest knots))) (rest (rest knots)))))]))
  (define (move-iter dirs knots tail-history)
    (cond
      [(empty? dirs) (cons (last knots) tail-history)]
      [else
       (let* ([new-head-pos (apply-move (first dirs) (first knots))]
              [new-knots (move-knots (cons new-head-pos (rest knots)))])
         (move-iter (rest dirs) new-knots (cons (last new-knots) tail-history)))]))
  (move-iter (individual-steps lst) (build-list num-knots (lambda (x) (cons 0 0))) empty))

(printf "Part 1: ~a\n" (length (remove-duplicates (last-knot-history input 2))))
(printf "Part 2: ~a\n" (length (remove-duplicates (last-knot-history input 10))))
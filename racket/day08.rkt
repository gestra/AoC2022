#lang racket

(define input (list->vector (map list->vector (map string->list (file->lines "../inputs/day08.txt")))))

(define test (list->vector (map list->vector (map string->list (list "30373"
                                                                     "25512"
                                                                     "65332"
                                                                     "33549"
                                                                     "35390")))))

(define (vis-row row)
  (define (iter-lr row i tallest seen)
    (cond
      [(>= i (vector-length row)) seen]
      [(char>? (vector-ref row i) tallest)
       (iter-lr row (+ 1 i) (vector-ref row i) (cons i seen))]
      [else (iter-lr row (+ 1 i) tallest seen)]))
  (define (iter-rl row i tallest seen)
    (cond
      [(< i 0) seen]
      [(char>? (vector-ref row i) tallest)
       (if (member i seen)
           (iter-rl row (- i 1) (vector-ref row i) seen)
           (iter-rl row (- i 1) (vector-ref row i) (cons i seen)))]
      [else (iter-rl row (- i 1) tallest seen)]))
     
  (iter-rl
   row
   (- (vector-length row) 1)
   (integer->char (- (char->integer #\0) 1))
   (iter-lr
    row
    0
    (integer->char (- (char->integer #\0) 1))
    empty)))

(define (collect-col vecs col-i)
  (build-vector (vector-length vecs) (lambda (x) (vector-ref (vector-ref vecs x) col-i))))

(define (solve-1 vec)
  (let ([seen empty])
    (for ([i (in-range (vector-length vec))])
      (set! seen (append (map (lambda (x) (cons i x)) (vis-row (vector-ref vec i))) seen)))
    (for ([j (in-range (vector-length (vector-ref vec 0)))])
      (set! seen (append (map (lambda (x) (cons x j)) (vis-row (collect-col vec j))) seen)))
    (length (remove-duplicates seen))))

(define (scenic-row row x)
  (let ([tree-height (vector-ref row x)]
        [left-score 0]
        [right-score 0])
    (for ([i (in-range (add1 x) (vector-length row))]
          #:final (char<=? tree-height (vector-ref row i)))
      (set! right-score (add1 right-score)))
    (for ([i (in-range (sub1 x) -1 -1)]
          #:final (char<=? tree-height (vector-ref row i)))
      (set! left-score (add1 left-score)))
    (* left-score right-score)))

(define (scenic-score vec x y)
  (*
   (scenic-row (vector-ref vec y) x)
   (scenic-row (collect-col vec x) y)))

(define (solve-2 vec)
  (apply max (for*/list ([y (in-range (vector-length vec))]
                         [x (in-range (vector-length (vector-ref vec 0)))])
               (scenic-score vec x y))))

(printf "Part 1: ~a\n" (solve-1 input))
(printf "Part 1: ~a\n" (solve-2 input))
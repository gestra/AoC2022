#lang racket

(define input
  (list->vector (filter (lambda (x) (or (char=? #\< x) (char=? #\> x)))
          (string->list (file->string "../inputs/day17.txt")))))
(define test (list->vector (string->list ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")))

(define shapes (list->vector (list
                              (list (cons 0 0) (cons 1 0) (cons 2 0) (cons 3 0)) ; -
                              (list (cons 1 2) (cons 0 1) (cons 1 1) (cons 2 1) (cons 1 0)) ; +
                              (list (cons 2 2) (cons 2 1) (cons 0 0) (cons 1 0) (cons 2 0)) ; Reverse L
                              (list (cons 0 3) (cons 0 2) (cons 0 1) (cons 0 0)) ; |
                              (list (cons 0 1) (cons 1 1) (cons 0 0) (cons 1 0))))) ; Square

(define (draw rocks shape-parts)
  (printf "\n")
  (define max-height (apply max (append (set-map rocks (lambda (x) (cdr x))) (map (lambda (x) (cdr x)) shape-parts))))
  (for ([y (in-range max-height -1 -1)])
    (for ([x (in-range 7)])
      (printf (cond
                [(set-member? rocks (cons x y)) "#"]
                [(member (cons x y) shape-parts) "@"]
                [else "."])))
    (printf "\n"))
  (printf "-------\n"))

(define (shape-all shape pos)
  (map (lambda (x) (cons (+ (car x) (car pos)) (+ (cdr x) (cdr pos)))) shape))

(define (collides? rocks shape-parts)
  (for/first ([p shape-parts]
              #:when (or (< (cdr p) 0) (< (car p) 0) (> (car p) 6) (set-member? rocks p)))
    #t))

(define (wind-push rocks shape wind pos)
  (let* ([new-pos (if (char=? wind #\<)
                      (cons (sub1 (car pos)) (cdr pos))
                      (cons (add1 (car pos)) (cdr pos)))]
         [shape-parts (shape-all shape new-pos)])
    (if (collides? rocks shape-parts)
        pos
        new-pos)))

(define (drop rocks shape pos)
  (let* ([new-pos (cons (car pos) (sub1 (cdr pos)))]
         [shape-parts (shape-all shape new-pos)])
    (if (collides? rocks shape-parts)
        pos
        new-pos)))

(define (new-rock rocks shape winds wind-i highest)
  (let ([pos (cons 2 (+ 3 highest))])
    (define (loop)
      ;(printf "Start loop\n")
      ;(draw rocks (shape-all shape pos))
      (set! pos (wind-push rocks shape (vector-ref winds wind-i) pos))
      ;(printf "After wind\n")
      ;(draw rocks (shape-all shape pos))
      (define after-drop (drop rocks shape pos))
      (cond
        [(equal? pos after-drop)
         (let ([shape-parts (shape-all shape after-drop)])
           (for-each (lambda (x) (set-add! rocks x)) shape-parts)
           ;(printf "After drop\n")
           ;(draw rocks empty)
           (let ([highest-dropped (add1 (apply max (map (lambda (x) (cdr x)) shape-parts)))])
             (if (> highest-dropped highest)
                 (cons highest-dropped wind-i)
                 (cons highest wind-i))))]
        [else
         (set! pos after-drop)
         (set! wind-i (if (>= (add1 wind-i) (vector-length winds)) 0 (add1 wind-i)))
         (loop)]))
  (loop)))
  
(define (simulate winds max-rocks)
  (let ([rocks (mutable-set)]
        [highest 0]
        [wind-i -1]
        [shape-i -1]
        [num-rocks 0])
    (define (loop)
      (set! wind-i (if (>= (add1 wind-i) (vector-length winds)) 0 (add1 wind-i)))
      (set! shape-i (if (>= (add1 shape-i) (vector-length shapes)) 0 (add1 shape-i)))
      (define drop-result (new-rock rocks (vector-ref shapes shape-i) winds wind-i highest))
      (set! highest (car drop-result))
      (set! wind-i (cdr drop-result))
      (set! num-rocks (add1 num-rocks))
      (cond
        [(>= num-rocks max-rocks) highest]
        [else (loop)]))
    (loop)))

(printf "Part 1: ~a\n" (simulate input 2022))
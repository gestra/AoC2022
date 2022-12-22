#lang racket

(define input (file->lines "../inputs/day22.txt"))
(define test (list "        ...#"
                   "        .#.."
                   "        #..."
                   "        ...."
                   "...#.......#"
                   "........#..."
                   "..#....#...."
                   "..........#."
                   "        ...#...."
                   "        .....#.."
                   "        .#......"
                   "        ......#."
                   ""
                   "10R5L5R10L4R5L5"))

(define (parse lst)
  (define board (list->vector (map (lambda (x) (list->vector (string->list x))) (take lst (- (length lst) 2)))))
  (define directions (last lst))
  (define starting-x (for/first ([i (in-range (vector-length board))] #:when (char=? #\. (vector-ref (vector-ref board 0) i))) i))
  (list board directions (cons starting-x 0)))

(define (split-turns lst)
  (cond
    [(empty? lst) empty]
    [(char-numeric? (first lst))
     (let* ([dir-i (for/first ([i (in-range (length lst))]
                               #:when (not (char-numeric? (list-ref lst i)))) i)]
            [num (if dir-i
                     (string->number (list->string (take lst dir-i)))
                     (string->number (list->string lst)))])
       (if dir-i
           (cons num (split-turns (list-tail lst dir-i)))
           (cons num empty)))]
    [else (cons (first lst) (split-turns (rest lst)))]))

(define (first-col row)
  (for/first ([i (in-range (vector-length row))]
              #:when (not (char=? #\SPACE (vector-ref row i))))
    i))

(define (last-col row)
  (for/first ([i (in-range (sub1 (vector-length row)) -1 -1)]
              #:when (not (char=? #\SPACE (vector-ref row i))))
    i))

(define (first-row board x)
  (for/first ([i (in-range (vector-length board))]
              #:when (and
                      (>= (vector-length (vector-ref board i)) (add1 x))
                      (not (char=? #\SPACE (vector-ref (vector-ref board i) x)))))
    i))

(define (last-row board x)
  (for/first ([i (in-range (sub1 (vector-length board)) -1 -1)]
              #:when (and
                      (>= (vector-length (vector-ref board i)) (add1 x))
                      (not (char=? #\SPACE (vector-ref (vector-ref board i) x)))))
    i))

(define (move board pos facing steps)
  (define x (car pos))
  (define y (cdr pos))
  (cond
    [(= 0 facing) ; Right
     (define row-v (vector-ref board y))
     (define can-move #t)
     (for ([_ (in-range steps)]
           #:break (not can-move))
       (define next-x (if (or (>= (add1 x) (vector-length row-v)) (char=? #\SPACE (vector-ref row-v (add1 x))))
                          (first-col row-v)
                          (add1 x)))
       (set! can-move #f)
       (when (not (char=? #\# (vector-ref row-v next-x))) (set! x next-x) (set! can-move #t)))]
    [(= 1 facing) ; Down
     (define can-move #t)
     (for ([_ (in-range steps)]
           #:break (not can-move))
       (define next-y (if (or
                           (>= (add1 y) (vector-length board))
                           (< (sub1 (vector-length (vector-ref board (add1 y)))) x)
                           (char=? #\SPACE (vector-ref (vector-ref board (add1 y)) x)))
                          (first-row board x)
                          (add1 y)))
       (set! can-move #f)
       (when (not (char=? #\# (vector-ref (vector-ref board next-y) x))) (set! y next-y) (set! can-move #t)))]
    [(= 2 facing) ; Left
     (define row-v (vector-ref board y))
     (define can-move #t)
     (for ([_ (in-range steps)]
           #:break (not can-move))
       (define next-x (if (or (< (sub1 x) 0) (char=? #\SPACE (vector-ref row-v (sub1 x))))
                          (last-col row-v)
                          (sub1 x)))
       (set! can-move #f)
       (when (not (char=? #\# (vector-ref row-v next-x))) (set! x next-x) (set! can-move #t)))]
    [(= 3 facing) ; Up
     (define can-move #t)
     (for ([_ (in-range steps)]
           #:break (not can-move))
       (define next-y (if (or
                           (< (sub1 y) 0)
                           (< (sub1 (vector-length (vector-ref board (sub1 y)))) x)
                           (char=? #\SPACE (vector-ref (vector-ref board (sub1 y)) x)))
                          (last-row board x)
                          (sub1 y)))
       (set! can-move #f)
       (when (not (char=? #\# (vector-ref (vector-ref board next-y) x))) (set! y next-y) (set! can-move #t)))])
  (cons x y))

(define (travel board starting-pos directions)
  (define pos starting-pos)
  (define facing 0)
  (for ([d directions])
    (cond
      [(number? d)
       (set! pos (move board pos facing d))]
      [else
       (set! facing (if (char=? #\L d)
                        (modulo (sub1 facing) 4)
                        (modulo (add1 facing) 4)))]))
    
  (cons pos facing))

(define (solve-1 board starting-pos directions)
  (define res (travel board starting-pos directions))
  (+ (* 1000 (add1 (cdr (car res)))) (* 4 (add1 (car (car res)))) (cdr res)))

(define start (parse input))

(define board (first start))
(define starting-pos (last start))
(define directions (split-turns (string->list (second start))))

(printf "Part 1: ~a\n" (solve-1 board starting-pos directions))
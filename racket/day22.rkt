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

(define (next-pos-cube pos facing size)
  (define x (car pos))
  (define y (cdr pos))
  (define new-facing #f)
  (define next (cond
                 [(= 0 facing) ; Right
                  (let ([xn (add1 x)])
                    (cond
                      [(= 0 (modulo xn size))
                       (cond
                         [(and (= xn (* 3 size)) (= 0 (floor (/ y size))))
                          (set! new-facing 2)
                          (cons (sub1 (* 2 size)) (sub1 (- (* 3 size) y)))]
                         [(= 1 (floor (/ y size)))
                          (set! new-facing 3)
                          (cons (+ size y) (sub1 size))]
                         [(and (= xn (* 2 size)) (= 2 (floor (/ y size))))
                          (set! new-facing 2)
                          (cons (sub1 (* 3 size)) (sub1 (- (* 3 size) y)))]
                         [(= 3 (floor (/ y size)))
                          (set! new-facing 3)
                          (cons (- y (* 2 size)) (sub1 (* 3 size)))]
                         [else (cons xn y)])]
                      [else (cons xn y)]))]
                 [(= 1 facing) ; Down
                  (let ([yn (add1 y)])
                    (cond
                      [(= 0 (modulo yn size))
                       (cond
                         [(and (= yn (* 4 size)) (= 0 (floor (/ x size))))
                          (set! new-facing 1)
                          (cons (+ x (* 2 size)) 0)]
                         [(and (= yn (* 3 size)) (= 1 (floor (/ x size))))
                          (set! new-facing 2)
                          (cons (sub1 size) (+ x (* 2 size)))]
                         [(= 2 (floor (/ x size)))
                          (set! new-facing 2)
                          (cons (sub1 (* 2 size)) (- x size))]
                         [else (cons x yn)])]
                      [else (cons x yn)]))]
                 [(= 2 facing) ; Left
                  (let ([xn (sub1 x)])
                          (cond
                            [(= (sub1 size) (modulo xn size))
                             (cond
                               [(and (= xn (sub1 size)) (= 0 (floor (/ y size))))
                                (set! new-facing 0)
                                (cons 0 (sub1 (- (* 3 size) y)))]
                               [(= 1 (floor (/ y size)))
                                (set! new-facing 1)
                                (cons (- y size) (* 2 size))]
                               [(and (= xn -1) (= 2 (floor (/ y size))))
                                (set! new-facing 0)
                                (cons size (sub1 (- (* 3 size) y)))]
                               [(= 3 (floor (/ y size)))
                                (set! new-facing 1)
                                (cons (- y (* 2 size)) 0)]
                               [else (cons xn y)])]
                            [else (cons xn y)]))]
                 [(= 3 facing) ; Up
                  (let ([yn (sub1 y)])
                    (cond
                      [(= (sub1 size) (modulo yn size))
                       (cond
                         [(and (= yn (sub1 (* 2 size))) (= 0 (floor (/ x size))))
                          (set! new-facing 0)
                          (cons size (+ x size))]
                         [(and (= yn -1) (= 1 (floor (/ x size))))
                          (set! new-facing 0)
                          (cons 0 (+ x (* 2 size)))]
                         [(= 2 (floor (/ x size)))
                          (set! new-facing 3)
                          (cons (- x (* 2 size)) (sub1 (* 4 size)))]
                         [else (cons x yn)])]
                      [else (cons x yn)]))]))
  (cons next new-facing))

(define (move-cube board pos facing steps size)
  (define x (car pos))
  (define y (cdr pos))
  (define stop? #f)
  (for ([_ (in-range steps)]
        #:break stop?)
    (define n (next-pos-cube (cons x y) facing size))
    (define next-pos (car n))
    (define new-facing (cdr n))
    
    
        
    (set! stop? #t)
    (when (not (char=? #\# (vector-ref (vector-ref board (cdr next-pos)) (car next-pos))))
      (set! x (car next-pos))
      (set! y (cdr next-pos))
      (when new-facing (set! facing new-facing))
      (set! stop? #f)))
  (cons (cons x y) facing))

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

(define (travel-cube board starting-pos directions size)
  (define pos starting-pos)
  (define facing 0)
  (for ([d directions])
    (cond
      [(number? d)
       (define res (move-cube board pos facing d size))
       (set! pos (car res))
       (set! facing (cdr res))]
      [else
       (set! facing (if (char=? #\L d)
                        (modulo (sub1 facing) 4)
                        (modulo (add1 facing) 4)))]))
    
  (cons pos facing))

(define (solve-1 board starting-pos directions)
  (define res (travel board starting-pos directions))
  (+ (* 1000 (add1 (cdr (car res)))) (* 4 (add1 (car (car res)))) (cdr res)))

(define (solve-2 board starting-pos directions size)
  (define res (travel-cube board starting-pos directions size))
  (+ (* 1000 (add1 (cdr (car res)))) (* 4 (add1 (car (car res)))) (cdr res)))

(define start (parse input))
(define size 50)

(define board (first start))
(define starting-pos (last start))
(define directions (split-turns (string->list (second start))))

(printf "Part 1: ~a\n" (solve-1 board starting-pos directions))
(printf "Part 2: ~a\n" (solve-2 board starting-pos directions size))
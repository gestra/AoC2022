#lang racket

(define input (file->lines "../inputs/day23.txt"))
(define test (list "....#.."
                   "..###.#"
                   "#...#.#"
                   ".#...##"
                   "#.###.."
                   "##.#.##"
                   ".#..#.."))
(define small-test (list "....."
                         "..##."
                         "..#.."
                         "....."
                         "..##."
                         "....."))

(define dir-order (list #\N #\S #\W #\E))

(define (parse-input lst)
  (define s (mutable-set))
  (for ([line lst]
        [y (in-range (length lst))])
    (for ([c (string->list line)]
          [x (in-range (string-length line))]
          #:when (char=? c #\#))
      (set-add! s (cons x y))))
  s)

(define (someone-around? elves elf)
  (define x (car elf))
  (define y (cdr elf))
  (for/first ([n (list (cons x (sub1 y))
                       (cons (add1 x) (sub1 y))
                       (cons (add1 x) y)
                       (cons (add1 x) (add1 y))
                       (cons x (add1 y))
                       (cons (sub1 x) (add1 y))
                       (cons (sub1 x) y)
                       (cons (sub1 x) (sub1 y)))]
              #:when (set-member? elves n))
    #t))

(define (gather-proposals elves first-dir)
  (define p empty)
  (for ([elf elves]
        #:when (someone-around? elves elf))
    (define proposed? #f)
    (define x (car elf))
    (define y (cdr elf))
    (for ([d-i (in-range 4)]
          #:break proposed?)
      (define dir (list-ref dir-order (modulo (+ first-dir d-i) 4)))
      (cond
        [(char=? dir #\N)
         (when (not (or (set-member? elves (cons (sub1 x) (sub1 y)))
                        (set-member? elves (cons x (sub1 y)))
                        (set-member? elves (cons (add1 x) (sub1 y)))))
           (set! proposed? #t)
           (set! p (cons (cons elf (cons x (sub1 y))) p)))]
        [(char=? dir #\S)
         (when (not (or (set-member? elves (cons (sub1 x) (add1 y)))
                        (set-member? elves (cons x (add1 y)))
                        (set-member? elves (cons (add1 x) (add1 y)))))
           (set! proposed? #t)
           (set! p (cons (cons elf (cons x (add1 y))) p)))]
        [(char=? dir #\W)
         (when (not (or (set-member? elves (cons (sub1 x) (sub1 y)))
                        (set-member? elves (cons (sub1 x) y))
                        (set-member? elves (cons (sub1 x) (add1 y)))))
           (set! proposed? #t)
           (set! p (cons (cons elf (cons (sub1 x) y)) p)))]
        [(char=? dir #\E)
         (when (not (or (set-member? elves (cons (add1 x) (sub1 y)))
                        (set-member? elves (cons (add1 x) y))
                        (set-member? elves (cons (add1 x) (add1 y)))))
           (set! proposed? #t)
           (set! p (cons (cons elf (cons (add1 x) y)) p)))])))
  p)

(define (round elves dir-i)
  (define props (gather-proposals elves dir-i))
  (define moves 0)
  (for ([p props])
    (let ([from (car p)]
          [to (cdr p)])
      (when (= 1 (count (lambda (x) (equal? (cdr x) to)) props))
        ;(printf "Elf at ~a,~a moves to ~a,~a\n" (car from) (cdr from) (car to) (cdr to))
        (set! moves (add1 moves))
        (set-remove! elves from)
        (set-add! elves to))))
  moves)

(define (extreme-coordinates elves)
  (define n +inf.f)
  (define s -inf.f)
  (define w +inf.f)
  (define e -inf.f)
  (for ([elf elves])
    (when (< (cdr elf) n) (set! n (cdr elf)))
    (when (> (cdr elf) s) (set! s (cdr elf)))
    (when (< (car elf) w) (set! w (car elf)))
    (when (> (car elf) e) (set! e (car elf))))
  (list n s w e))

(define (calculate-empty-tiles elves rectangle)
  (for/sum ([y (in-range (first rectangle) (add1 (second rectangle)))])
    (for/sum ([x (in-range (third rectangle) (add1 (fourth rectangle)))]
              #:when (not (set-member? elves (cons x y))))
      1)))

(define (print-map elves)
  (define limits (extreme-coordinates elves))
  (for ([y (in-range (first limits) (add1 (second limits)))])
    (for ([x (in-range (third limits) (add1 (fourth limits)))])
      (printf (if (set-member? elves (cons x y)) "#" ".")))
    (printf "\n")))

(define (solve-1 elves)
  (for ([r (in-range 10)])
    (round elves r))
    ;(printf "After round ~a:\n" (add1 r))
    ;(print-map elves))
  (calculate-empty-tiles elves (extreme-coordinates elves)))

(define (solve-2 elves)
  (define rounds -1)
  (define (loop)
    (set! rounds (add1 rounds))
    (when (not (= 0 (round elves rounds))) (loop)))
  (loop)
  (add1 rounds))

(printf "Part 1: ~a\n" (solve-1 (parse-input input)))
(printf "Part 2: ~a\n" (solve-2 (parse-input input)))
  
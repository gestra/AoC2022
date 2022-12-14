#lang racket

(define input (file->lines "../inputs/day14.txt"))
(define test (list
              "498,4 -> 498,6 -> 496,6"
              "503,4 -> 502,4 -> 502,9 -> 494,9"))

(define (coords-between c1 c2)
  (cond
    [(= (car c1) (car c2))
     (let ([r (if (< (cdr c1) (cdr c2))
                  (range (cdr c1) (add1 (cdr c2)))
                  (range (cdr c2) (add1 (cdr c1))))])
       (map (lambda (x) (cons (car c1) x)) r))]
    [else
     (let ([r (if (< (car c1) (car c2))
                  (range (car c1) (add1 (car c2)))
                  (range (car c2) (add1 (car c1))))])
       (map (lambda (x) (cons x (cdr c1))) r))]))

(define (parse-input lst)
  (define (parse-line line)
    (define (c-to-num c-s)
      (let ([l (map string->number (string-split c-s ","))])
        (cons (first l) (second l))))
    (let ([ends (map c-to-num (string-split line " -> "))])
      (define (parse-num lst)
        (cond
          [(<= (length lst) 1) empty]
          [else (append (coords-between (first lst) (second lst)) (parse-num (rest lst)))]))
      (parse-num ends)))

  (cond
    [(empty? lst) empty]
    [else (append (parse-line (first lst)) (parse-input (rest lst)))]))

(define (step rocks sand pos [floor #f])
  (define (can-drop-to? pos)
    (if floor
        (not (or (set-member? rocks pos) (set-member? sand pos) (= (cdr pos) floor)))
        (not (or (set-member? rocks pos) (set-member? sand pos)))))
  (let ([drop-pos (list
                   (cons (car pos) (add1 (cdr pos)))
                   (cons (sub1 (car pos)) (add1 (cdr pos)))
                   (cons (add1 (car pos)) (add1 (cdr pos))))])
    (cond
      [(can-drop-to? (first drop-pos)) (first drop-pos)]
      [(can-drop-to? (second drop-pos)) (second drop-pos)]
      [(can-drop-to? (third drop-pos)) (third drop-pos)]
      [else pos])))

(define (above-abyss? rocks sand pos)
  (not (for/first ([y (in-range (cdr pos) (+ (cdr pos) 500))]
                   #:when (or (set-member? rocks (cons (car pos) y)) (set-member? sand (cons (car pos) y))))
         #t)))

(define (new-sand rocks sand [floor #f])
  (let ([pos (cons 500 0)])
    (define (loop)
      (let ([new-pos (step rocks sand pos floor)])
        (cond
          [(and (not floor) (above-abyss? rocks sand pos)) sand]
          [(equal? pos new-pos) (set-add! sand pos) sand]
          [else (set! pos new-pos) (loop)])))
    (loop)))

(define (simulate rocks [floor #f])
  (let ([sand (mutable-set)])
    (define (loop)
      (define old-count (set-count sand))
      (new-sand rocks sand floor)
      (cond
        [(= old-count (set-count sand)) sand]
        [else (loop)]))
    (loop)))

(define rocks (list->set (parse-input input)))
(define floor (+ 2 (apply max (set-map rocks (lambda (x) (cdr x))))))

(printf "Part 1: ~a\n" (set-count (simulate rocks)))
(printf "Part 2: ~a\n" (set-count (simulate rocks floor)))

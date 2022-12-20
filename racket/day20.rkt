#lang racket

(define input (map string->number (file->lines "../inputs/day20.txt")))
(define test (list 1 2 -3 3 -2 0 4))

(struct num (val pos) #:transparent)

(define (init lst)
  (for/vector ([n lst]
               [p (in-range (length lst))])
    (num n p)))

(define (print-in-order vec)
  (define sorted (vector-sort vec (lambda (x y) (< (num-pos x) (num-pos y)))))
  (for ([s sorted])
    (printf "~a, " (num-val s))))

(define (move vec i)
  (define orig-pos (num-pos (vector-ref vec i)))
  (define val (num-val (vector-ref vec i)))
  
  (define new-pos (modulo (+ val orig-pos) (sub1 (vector-length vec))))
  (vector-set! vec i (num val new-pos))
  
  (for ([n vec]
        [j (in-range (vector-length vec))]
        #:when (if (> new-pos orig-pos)
                   (and (not (= i j)) (> (num-pos n) orig-pos) (<= (num-pos n) new-pos))
                   (and (not (= i j)) (< (num-pos n) orig-pos) (>= (num-pos n) new-pos))))
    (if (> new-pos orig-pos)
        (vector-set! vec j (num (num-val n) (sub1 (num-pos n))))
        (vector-set! vec j (num (num-val n) (add1 (num-pos n)))))))

(define (get-pos vec pos)
  (for/first ([n vec]
              #:when (= (num-pos n) (modulo pos (vector-length vec))))
    (num-val n)))

(define (mix vec)
  (for ([i (in-range (vector-length vec))])
    (move vec i))
  vec)

(define (find-zero vec)
  (for/first ([n vec]
              #:when (= 0 (num-val n)))
    n))
    
(define (solve-1 vec)
  (mix vec)
  (define zero-pos (num-pos (find-zero vec)))
  (+ (get-pos vec (+ 1000 zero-pos)) (get-pos vec (+ 2000 zero-pos)) (get-pos vec (+ 3000 zero-pos))))

(define (solve-2 orig-vec)
  (define vec (vector-map (lambda (x) (num (* (num-val x) 811589153) (num-pos x))) orig-vec))
  (for ([_ (in-range 10)])
    (mix vec))
  (define zero-pos (num-pos (find-zero vec)))
  (+ (get-pos vec (+ 1000 zero-pos)) (get-pos vec (+ 2000 zero-pos)) (get-pos vec (+ 3000 zero-pos))))

(printf "Part 1: ~a\n" (solve-1 (init input)))
(printf "Part 1: ~a\n" (solve-2 (init input)))
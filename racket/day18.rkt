#lang racket

(define input (file->lines "../inputs/day18.txt"))
(define test (list "2,2,2"
                   "1,2,2"
                   "3,2,2"
                   "2,1,2"
                   "2,3,2"
                   "2,2,1"
                   "2,2,3"
                   "2,2,4"
                   "2,2,6"
                   "1,2,5"
                   "3,2,5"
                   "2,1,5"
                   "2,3,5"))

(define (parse lst)
  (for/list ([c lst])
    (map string->number (string-split c ","))))

(define neighbor-dirs (list (list -1 0 0) (list 0 -1 0) (list 0 0 -1) (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (neighbors pos)
  (map
   (lambda (x) (list (+ (first pos) (first x)) (+ (second pos) (second x)) (+ (third pos) (third x))))
   neighbor-dirs))

(define (sides-exposed cubes pos)
  (for/list ([n (neighbors pos)]
            #:when (not (set-member? cubes n)))
    n))

(define (solve-1 cubes)
  (for/sum ([c cubes])
    (length (sides-exposed cubes c))))

(define (min-max-coords cubes)
  (let ([min-c +inf.f]
        [max-c -inf.f])
    (for ([cube cubes])
      (for ([coord cube])
        (when (< coord min-c) (set! min-c coord))
        (when (> coord max-c) (set! max-c coord))))
    (cons min-c max-c)))

(define (outside-limits? limits pos)
  (for/first ([c pos]
              #:when (or (< c (car limits)) (> c (cdr limits))))
    #t))

(define (is-enclosed? cubes pos limits enclosed-spaces)
  (define limit-reached? #f)
  (define (fill p s)
    (cond
      [(set-member? enclosed-spaces p) s]
      [limit-reached? s]
      [(set-member? cubes p) s]
      [(outside-limits? limits p) (set! limit-reached? #t) s]
      [else
       (set-add! s p)
       (for ([n (neighbors p)]
             #:when (not (set-member? s n)))
         (fill n s))
       s]))
  (let ([s (fill pos (mutable-set))])
    (when (not limit-reached?) (set-union! enclosed-spaces s))
    (if limit-reached?
        #f
        s)))

(define (exterior-sides-exposed cubes pos limits enclosed-spaces)
  (for/list ([n (neighbors pos)]
            #:when (and (not (set-member? cubes n)) (not (is-enclosed? cubes n limits enclosed-spaces))))
    n))

(define (solve-2 cubes)
  (define limits (min-max-coords cubes))
  (define enclosed-spaces (mutable-set))
  (for/sum ([c cubes])
    (length (exterior-sides-exposed cubes c limits enclosed-spaces))))

(define s (list->set (parse input)))
(printf "Part 1: ~a\n" (solve-1 s))
(printf "Part 2: ~a\n" (solve-2 s))
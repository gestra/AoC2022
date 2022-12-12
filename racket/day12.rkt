#lang racket

(define input (file->lines "../inputs/day12.txt"))
(define test (list
              "Sabqponm"
              "abcryxxl"
              "accszExk"
              "acctuvwj"
              "abdefghi"))

(define (parse-heightmap lst)
  (define (parse-line chars)
    (define (mark->int c)
      (cond
        [(char=? c #\S) 0]
        [(char=? c #\E) 25]
        [else (- (char->integer c) 97)]))
    (cond
      [(empty? chars) empty]
      [else (cons (mark->int (first chars)) (parse-line (rest chars)))]))
  (cond
    [(empty? lst) empty]
    [else (cons (parse-line (string->list (first lst))) (parse-heightmap (rest lst)))]))

(define (find-endpoints lst)
  (let ([start #f]
        [end #f])
    (for* ([y (in-range (length lst))]
           [x (in-range (length (string->list (first lst))))]
           #:break (and start end))
      (cond
        [(char=? #\S (list-ref (string->list (list-ref lst y)) x)) (set! start (cons x y))]
        [(char=? #\E (list-ref (string->list (list-ref lst y)) x)) (set! end (cons x y))]))
    (cons start end)))

(define (A-star start end heightmap)
  (define (d current target)
    (cond
      [(>=
        (add1 (vector-ref (vector-ref heightmap (cdr current)) (car current)))
        (vector-ref (vector-ref heightmap (cdr target)) (car target)))
       1]
      [else +inf.f]))

  (define (h pos)
    (+ (abs (- (car pos) (car end))) (abs (- (cdr pos) (cdr end)))))
  
  (let ([openset (mutable-set start)]
        [camefrom (make-hash)]
        [gscore (make-hash)]
        [fscore (make-hash)])
    (hash-set! gscore start 0)
    (hash-set! fscore start (h start))
    
    (define (find-loop)
      (define (node-with-lowest-fscore-in-openset)
        (let ([lowest-node (first (set->list openset))]
              [lowest-val +inf.f])
          (for ([n (set->list openset)]
                #:when (< (hash-ref fscore n) lowest-val))
               (set! lowest-val (hash-ref fscore n))
               (set! lowest-node n))
          lowest-node))
      
      (define (reconstruct-path current)
        (let ([totalpath (list current)])
          (define (rc-loop)
            (set! current (hash-ref camefrom current))
            (set! totalpath (cons current totalpath))
            (if (hash-has-key? camefrom current)
                (rc-loop)
                totalpath))
          (rc-loop)))
          
      (let ([current (node-with-lowest-fscore-in-openset)])
        (cond
          [(and (= (car current) (car end)) (= (cdr current) (cdr end)))
           (reconstruct-path current)]
          [else
           (set-remove! openset current)
           (for* ([neighbor (list (cons (sub1 (car current)) (cdr current))
                                  (cons (add1 (car current)) (cdr current))
                                  (cons (car current) (sub1 (cdr current)))
                                  (cons (car current) (add1 (cdr current))))]
                  #:when (not (or
                               (< (car neighbor) 0)
                               (< (cdr neighbor) 0)
                               (>= (cdr neighbor) (vector-length heightmap))
                               (>= (car neighbor) (vector-length (vector-ref heightmap 0))))))
             (let ([tentative-gscore (+ (hash-ref gscore current) (d current neighbor))])
               (cond
                 [(< tentative-gscore (if (hash-has-key? gscore neighbor)
                                          (hash-ref gscore neighbor)
                                          +inf.f))
                  (hash-set! camefrom neighbor current)
                  (hash-set! gscore neighbor tentative-gscore)
                  (hash-set! fscore neighbor (+ tentative-gscore (h neighbor)))
                  (set-add! openset neighbor)])))
           (cond [(not (set-empty? openset)) (find-loop)])])))
    (find-loop)))

(define (min-steps start end vecs)
  (let ([steps (A-star start end vecs)])
    (if (list? steps)
        (sub1 (length steps))
        #f)))
    
(define (solve-1 lst)
  (let ([endpoints (find-endpoints lst)]
        [vecs (list->vector (map list->vector (parse-heightmap lst)))])
    (min-steps (car endpoints) (cdr endpoints) vecs)))

(define (solve-2 lst)
  (let ([vecs (list->vector (map list->vector (parse-heightmap lst)))]
        [endpoints (find-endpoints lst)])
    (define (find-as)
      (for*/list ([y (in-range (vector-length vecs))]
                  [x (in-range (vector-length (vector-ref vecs 0)))]
                  #:when (= 0 (vector-ref (vector-ref vecs y) x)))
        (cons x y)))
    (first (sort (filter number? (map (lambda (x) (min-steps x (cdr endpoints) vecs)) (find-as))) <))))

(printf "Part 1: ~a\n" (solve-1 input))
(printf "Part 2: ~a\n" (solve-2 input))
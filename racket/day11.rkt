#lang racket

(define input (file->lines "../inputs/day11.txt"))
(define test (list
              "Monkey 0:"
              "  Starting items: 79, 98"
              "  Operation: new = old * 19"
              "  Test: divisible by 23"
              "    If true: throw to monkey 2"
              "    If false: throw to monkey 3"
              ""
              "Monkey 1:"
              "  Starting items: 54, 65, 75, 74"
              "  Operation: new = old + 6"
              "  Test: divisible by 19"
              "    If true: throw to monkey 2"
              "    If false: throw to monkey 0"
              ""
              "Monkey 2:"
              "  Starting items: 79, 60, 97"
              "  Operation: new = old * old"
              "  Test: divisible by 13"
              "    If true: throw to monkey 1"
              "    If false: throw to monkey 3"
              ""
              "Monkey 3:"
              "  Starting items: 74"
              "  Operation: new = old + 3"
              "  Test: divisible by 17"
              "    If true: throw to monkey 0"
              "    If false: throw to monkey 1"))

(struct monkey (number items operation divisibility-test-val targets num-inspected) #:transparent)

(define (parse-input lst)
  (define (split-monkeys lst)
    (let ([i (index-of lst "")])
      (if i
          (cons (take lst i) (split-monkeys (drop lst (add1 i))))
          (list lst))))
  (define (parse-monkeys lst)
    (cond
      [(empty? lst) empty]
      [else (cons (parse-monkey (first lst)) (parse-monkeys (rest lst)))]))
  (define (parse-monkey lst)
    (define (parse-operation str)
      (let* ([parts (regexp-match #rx"= old (.) (.+)" str)]
             [op (list-ref parts 1)]
             [rhs (string->number (list-ref parts 2))])
        (cond
          [(string=? "+" op) (if (number? rhs)
                                 (lambda (x) (+ x rhs))
                                 (lambda (x) (+ x x)))]
          [(string=? "-" op) (if (number? rhs)
                                 (lambda (x) (- x rhs))
                                 (lambda (x) (- x x)))]
          [(string=? "*" op) (if (number? rhs)
                                 (lambda (x) (* x rhs))
                                 (lambda (x) (* x x)))]
          [(string=? "/" op) (if (number? rhs)
                                 (lambda (x) (/ x rhs))
                                 (lambda (x) (/ x x)))])))
    (let ([number (string->number (list-ref (regexp-match #rx"Monkey ([0-9]+):" (list-ref lst 0)) 1))]
          [items (map string->number (string-split (list-ref (regexp-match #rx"items: (.+)" (list-ref lst 1)) 1) ", "))]
          [operation (parse-operation (list-ref lst 2))]
          [test? (string->number (list-ref (regexp-match #rx"by ([0-9]+)" (list-ref lst 3)) 1))]
          [target-t (string->number (list-ref (regexp-match #rx"to monkey ([0-9]+)" (list-ref lst 4)) 1))]
          [target-f (string->number (list-ref (regexp-match #rx"to monkey ([0-9]+)" (list-ref lst 5)) 1))])
      (monkey number items operation test? (cons target-t target-f) 0)))

  (parse-monkeys (split-monkeys lst)))

(define (apply-rounds monkeys num-rounds relief? supermodulo)
  (define (apply-turn monkeys i)
    (let ([m (list-ref monkeys i)]
          [new-items (make-list (length monkeys) empty)])
      (for ([item (monkey-items m)])
        (let* ([new-worry (if relief?
                              (floor (/ ((monkey-operation m) item) 3))
                              (modulo ((monkey-operation m) item) supermodulo))]
               [receiver (if (= (remainder new-worry (monkey-divisibility-test-val m)) 0)
                             (car (monkey-targets m))
                             (cdr (monkey-targets m)))])
          (set! new-items (list-set new-items receiver (cons new-worry (list-ref new-items receiver))))))
      (build-list
       (length monkeys)
       (lambda (x)
         (cond
           [(= x i)
            (monkey
             (monkey-number (list-ref monkeys x))
             empty
             (monkey-operation (list-ref monkeys x))
             (monkey-divisibility-test-val (list-ref monkeys x))
             (monkey-targets (list-ref monkeys x))
             (+ (monkey-num-inspected (list-ref monkeys x)) (length (monkey-items (list-ref monkeys x)))))]
           [else
            (monkey
             (monkey-number (list-ref monkeys x))
             (append (monkey-items (list-ref monkeys x)) (reverse (list-ref new-items x)))
             (monkey-operation (list-ref monkeys x))
             (monkey-divisibility-test-val (list-ref monkeys x))
             (monkey-targets (list-ref monkeys x))
             (monkey-num-inspected (list-ref monkeys x)))])))))
  (cond
    [(<= num-rounds 0) monkeys]
    [else
     (apply-rounds
      (let ([new-monkeys monkeys])
        (for ([i (in-range (length monkeys))])
          (set! new-monkeys (apply-turn new-monkeys i)))
        new-monkeys)
      (sub1 num-rounds)
      relief?
      supermodulo)]))

(define (solve monkeys rounds relief?)
  (let* ([supermodulo (apply * (build-list (length monkeys) (lambda (x) (monkey-divisibility-test-val (list-ref monkeys x)))))]
         [result (apply-rounds monkeys rounds relief? supermodulo)]
         [inspected (sort (build-list (length monkeys) (lambda (x) (monkey-num-inspected (list-ref result x)))) >)])
    (* (first inspected) (second inspected))))

(printf "Part 1: ~a\n" (solve (parse-input input) 20 #t))
(printf "Part 2: ~a\n" (solve (parse-input input) 10000 #f))
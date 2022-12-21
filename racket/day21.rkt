#lang racket

(define input (file->lines "../inputs/day21.txt"))
(define test (list "root: pppw + sjmn"
                   "dbpl: 5"
                   "cczh: sllz + lgvd"
                   "zczc: 2"
                   "ptdq: humn - dvpt"
                   "dvpt: 3"
                   "lfqf: 4"
                   "humn: 5"
                   "ljgn: 2"
                   "sjmn: drzm * dbpl"
                   "sllz: 4"
                   "pppw: cczh / lfqf"
                   "lgvd: ljgn * ptdq"
                   "drzm: hmdt - zczc"
                   "hmdt: 32"))

(define (parse lst)
  (define h (make-hash))
  (for ([line lst])
    (let ([re (regexp-match #rx"^([a-z]+): ([0-9-]+|.+)$" line)])
      (cond
        [(string->number (list-ref re 2)) (hash-set! h (list-ref re 1) (string->number (list-ref re 2)))]
        [else (hash-set! h (list-ref re 1) (list-ref re 2))])))
  h)

(define (solve-1 h)
  (define (loop)
    (for ([(monkey num) h]
          #:when (string? num))
      (let* ([re (regexp-match #rx"^([a-z]+) (.) ([a-z]+)$" num)]
             [lhs (hash-ref h (list-ref re 1))]
             [op (list-ref re 2)]
             [rhs (hash-ref h (list-ref re 3))])
        (when (and (number? lhs) (number? rhs))
          (define res (cond
                        [(string=? "+" op) (+ lhs rhs)]
                        [(string=? "-" op) (- lhs rhs)]
                        [(string=? "*" op) (* lhs rhs)]
                        [(string=? "/" op) (/ lhs rhs)]))
          (hash-set! h monkey res))))
    (when (not (number? (hash-ref h "root"))) (loop)))
  (loop)
  (hash-ref h "root"))

(printf "Part 1: ~a\n" (solve-1 (parse test)))
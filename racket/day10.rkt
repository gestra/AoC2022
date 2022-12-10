#lang racket

(define input (file->lines "../inputs/day10.txt"))
(define test (list "noop" "addx 3" "addx -5"))
(define large-test (list "addx 15" "addx -11" "addx 6" "addx -3" "addx 5" "addx -1" "addx -8" "addx 13" "addx 4" "noop" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx -35" "addx 1" "addx 24" "addx -19" "addx 1" "addx 16" "addx -11" "noop" "noop" "addx 21" "addx -15" "noop" "noop" "addx -3" "addx 9" "addx 1" "addx -3" "addx 8" "addx 1" "addx 5" "noop" "noop" "noop" "noop" "noop" "addx -36" "noop" "addx 1" "addx 7" "noop" "noop" "noop" "addx 2" "addx 6" "noop" "noop" "noop" "noop" "noop" "addx 1" "noop" "noop" "addx 7" "addx 1" "noop" "addx -13" "addx 13" "addx 7" "noop" "addx 1" "addx -33" "noop" "noop" "noop" "addx 2" "noop" "noop" "noop" "addx 8" "noop" "addx -1" "addx 2" "addx 1" "noop" "addx 17" "addx -9" "addx 1" "addx 1" "addx -3" "addx 11" "noop" "noop" "addx 1" "noop" "addx 1" "noop" "noop" "addx -13" "addx -19" "addx 1" "addx 3" "addx 26" "addx -30" "addx 12" "addx -1" "addx 3" "addx 1" "noop" "noop" "noop" "addx -9" "addx 18" "addx 1" "addx 2" "noop" "noop" "addx 9" "noop" "noop" "noop" "addx -1" "addx 2" "addx -37" "addx 1" "addx 3" "noop" "addx 15" "addx -21" "addx 22" "addx -6" "addx 1" "noop" "addx 2" "addx 1" "noop" "addx -10" "noop" "noop" "addx 20" "addx 1" "addx 2" "addx 2" "addx -6" "addx -11" "noop" "noop" "noop"))

(define (process lst)
  (define wanted-cycles (list 20 60 100 140 180 220))
  (let ([x 1]
        [cycle 0]
        [signal-strengths empty])
    (for ([cmd lst])
      (set! cycle (add1 cycle))
      (cond
        [(string=? "noop" cmd)
         (set! signal-strengths (if (member cycle wanted-cycles)
                                    (cons (* cycle x) signal-strengths)
                                    signal-strengths))]
        [else
         (let ([val (string->number (list-ref (regexp-match #rx".+ ([0-9-]+)" cmd) 1))])
           (set! signal-strengths (if (member cycle wanted-cycles)
                                      (cons (* cycle x) signal-strengths)
                                      signal-strengths))
           (set! cycle (add1 cycle))
           (set! signal-strengths (if (member cycle wanted-cycles)
                                      (cons (* cycle x) signal-strengths)
                                      signal-strengths))
           (set! x (+ x val)))]))
    signal-strengths))

(define (process-crt lst)
  (define (draw cycle x)
    (if (<= (abs (- (remainder cycle 40) (add1 x))) 1)
        (printf "#")
        (printf "."))
    (if (= 0 (remainder cycle 40))
        (printf "\n")
        (printf "")))
  (let ([x 1]
        [cycle 0])
    (for ([cmd lst])
      (set! cycle (add1 cycle))
      (cond
        [(string=? "noop" cmd)
         (draw cycle x)]
        [else
         (let ([val (string->number (list-ref (regexp-match #rx".+ ([0-9-]+)" cmd) 1))])
           (draw cycle x)
           (set! cycle (add1 cycle))
           (draw cycle x)
           (set! x (+ x val)))]))))

(printf "Part 1: ~a\n" (apply + (process input)))
(printf "Part 2:\n") (process-crt input)
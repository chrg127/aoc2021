(define (modulo-1 i m n) (add1 (modulo (sub1 (+ i m)) n)))
(define (roll-3 s) (+ (modulo-1 s 1 100) (modulo-1 s 2 100) (modulo-1 s 3 100)))

(define (deterministic-game p1 p2 score1 score2 die steps)
  (let* ([p1roll (roll-3 die)]
         [p2roll (roll-3 (modulo-1 die 3 100))]
         [p1pos (modulo-1 p1 p1roll 10)]
         [p2pos (modulo-1 p2 p2roll 10)]
         [p1score (+ score1 p1pos)]
         [p2score (+ score2 p2pos)])
    (cond ((>= p1score 1000) (values p1score score2  (+ steps 3)))
          ((>= p2score 1000) (values p1score p2score (+ steps 6)))
          (else
            (deterministic-game p1pos p2pos p1score p2score
                                (modulo-1 die 6 100)
                                (+ steps 6))))))

(define (sol1 init-pos)
  (define-values (s1 s2 steps) (deterministic-game (car init-pos) (cadr init-pos) 0 0 0 0))
  (define losing (if (>= s1 1000) s2 s1))
  (displayln (* losing steps)))

(define (roll->num-unis roll)
  (cond ((= roll 3) 1)
        ((= roll 4) 3)
        ((= roll 5) 6)
        ((= roll 6) 7)
        ((= roll 7) 6)
        ((= roll 8) 3)
        ((= roll 9) 1)))

(define (vec+ l m) (list (+ (car l) (car m)) (+ (cadr l) (cadr m))))
(define (vec* l s) (list (* s (car l)) (* s (cadr l))))

(define (dirac-game init-pos)
  (define memo (make-hash))
  (define (loop pos scores p roll step)
    (let ([memo-res (hash-ref memo (list pos scores p roll) (lambda () #f))])
      (if memo-res
        memo-res
        (let* ([new-pos (list-update pos p (lambda (x) (modulo-1 x roll 10)))]
               [new-scores (list-update scores p (lambda (x) (+ x (list-ref new-pos p))))]
               [next-p (if (= p 0) 1 0)])
          (cond ((>= (car  new-scores) 21) (list (roll->num-unis roll) 0))
                ((>= (cadr new-scores) 21) (list 0 (roll->num-unis roll)))
                (else
                  (let* ([res1 (foldl (lambda (x r) (vec+ r (loop new-pos new-scores next-p x (add1 step))))
                                      '(0 0) '(3 4 5 6 7 8 9))]
                         [res2 (vec* res1 (roll->num-unis roll))])
                    (hash-set! memo (list pos scores p roll) res2)
                    res2)))))))
  (foldl (lambda (x r) (vec+ r (loop init-pos '(0 0) 0 x 0))) '(0 0) '(3 4 5 6 7 8 9)))

(define (sol2 positions)
  (displayln (apply max (dirac-game positions))))

(sol1 '(4 8))
(sol1 '(9 10))
(sol2 '(4 8))
(sol2 '(9 10))

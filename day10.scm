(require math)

(define *stack* '())
(define (stack-push! x) (set! *stack* (cons x *stack*)))
(define (stack-pop!) (let ((x (car *stack*))) (set! *stack* (cdr *stack*)) x))
(define (stack-clear!) (set! *stack* '()))
(define (parse name) (map string->list (file->lines name)))
(define (open-paren? x) (or (char=? x #\() (char=? x #\[) (char=? x #\{) (char=? x #\<)))
(define points (hash #\) 3 #\] 57 #\} 1197 #\> 25137))
(define points-part2 (hash 'curved 1 'square 2 'curly 3 'angle 4))
(define ptype (hash #\( 'curved #\) 'curved #\[ 'square #\] 'square #\{ 'curly #\} 'curly #\< 'angle #\> 'angle))

(define (loop parens success-fn error-fn)
  (if (null? parens)
    (success-fn)
    (let ((p (car parens)))
      (cond ((open-paren? p) (begin (stack-push! (hash-ref ptype p)) (loop (cdr parens) success-fn error-fn)))
            ((not (eq? (hash-ref ptype p) (stack-pop!))) (error-fn p))
            (else (loop (cdr parens) success-fn error-fn))))))

(define (check-syntax parens)
  (stack-clear!)
  (loop parens (lambda () 0) (lambda (p) (hash-ref points p))))

(define (parens-to-points lst)
  (foldl (lambda (p r) (+ (* r 5) (hash-ref points-part2 p))) 0 lst))

(define (check-syntax2 parens)
  (stack-clear!)
  (parens-to-points (loop parens (lambda () *stack*) (lambda (p) '()))))

(define (sol1 name)
  (displayln (apply + (map check-syntax (parse name)))))

(define (sol2 name)
  (displayln (median < (sort (filter (lambda (x) (not (= x 0))) (map check-syntax2 (parse name))) <))))

(sol1 "input10-1.txt")
(sol1 "input10-2.txt")
(sol2 "input10-1.txt")
(sol2 "input10-2.txt")

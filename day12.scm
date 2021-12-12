(define (graph-add-path! g p) (begin (hash-update! g (car  p) (lambda (x) (cons (cadr p) x)) (lambda () '()))
                                     (hash-update! g (cadr p) (lambda (x) (cons (car  p) x)) (lambda () '()))))
(define (parse name)
  (define graph (make-hash))
  (for-each (lambda (p) (graph-add-path! graph p))
            (map (lambda (l) (string-split l "-")) (file->lines name)))
  graph)

(define (small? cave) (andmap char-lower-case? (string->list cave)))

(define (visit graph node locked locked?)
  (if (string=? node "end")
    1
    (let* ([new-locked (if (small? node)
                        (hash-update locked node add1 (lambda () 0))
                        locked)]
           [options (filter (lambda (x) (not (locked? x (hash-ref new-locked x (lambda () 0)) new-locked)))
                           (hash-ref graph node))])
      (foldl (lambda (n r) (+ r (visit graph n new-locked locked?))) 0 options))))

(define (sol1 name)
  (displayln (visit (parse name) "start" (hash) (lambda (k v t) (= v 1)))))

(define (sol2 name)
  (define (cave-locked? k v t)
    (or (and (string=? k "start") (= v 1))
        (>= v (if (findf (lambda (x) (= x 2)) (hash-values t)) 1 2))))
  (displayln (visit (parse name) "start" (hash) cave-locked?)))

(sol1 "input12-1.txt")
(sol1 "input12-3.txt")
(sol1 "input12-4.txt")
(sol1 "input12-2.txt")

(sol2 "input12-1.txt")
(sol2 "input12-3.txt")
(sol2 "input12-4.txt")
(sol2 "input12-2.txt")

; (define (make-paths graph node locked locked?)
;   (if (string=? node "end")
;     (list (list node))
;     (let ([new-locked (if (small? node) (hash-update locked node add1 (lambda () 0)) locked)]
;           [options (filter (lambda (x) (not (locked? x (hash-ref locked x (lambda () 0)) locked)))
;                            (hash-ref graph node))])
;       (map (lambda (p) (cons node p))
;            (apply append
;                   (map (lambda (n) (make-paths graph n new-locked locked?)) options))))))

; (define (display-paths name locked-pred)
;   (define paths (make-paths (parse name) "start" (hash) locked-pred))
;   (define paths-str (sort (map (lambda (p)
;                                  (apply string-append
;                                         (map (lambda (n) (format "~a," n)) p)))
;                                paths)
;                           string<?))
;   (for-each (lambda (p) (displayln (substring p 0 (sub1 (string-length p)))))
;             paths-str))

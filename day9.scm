(define (char->num c) (- (char->integer c) (char->integer #\0)))
(define (zip . lst) (apply map list lst))
(define (make-adjs i j) (zip (list (sub1 i) (add1 i) i i) (list j j (sub1 j) (add1 j))))
(define (map-index f lst)
  (define (step lst f i)
    (if (null? lst) '() (cons (f (car lst) i) (step (cdr lst) f (+ i 1)))))
  (step lst f 0))

(define (parse name)
  (map (lambda (x) (map char->num (string->list x))) (file->lines name)))

(define (get-tab name)
  (define tab (make-hash))
  (map-index (lambda (l i)
               (map-index (lambda (x j)
                            (hash-set! tab (list i j) x)) l)) (parse name))
  tab)

(define (low-points tab)
  (define (is-low? pos v)
    (let ([adj (map (lambda (x) (hash-ref tab x (lambda () 9))) (make-adjs (car pos) (cadr pos)))])
      (= (count (lambda (x) (< v x)) adj) 4)))
  (filter (lambda (x) (not (eq? x -1)))
          (hash-map tab (lambda (k v) (if (is-low? k v) k -1)))))

(define (sol1 name)
  (define *tab* (get-tab name))
  (displayln (apply + (map (lambda (p) (+ (hash-ref *tab* p) 1)) (low-points *tab*)))))

(define (sol2 name)
  (define (basin-size tab pos)
    (define memo (make-hash))
    (define (visit pos)
      (if (hash-has-key? memo pos)
        'already-visited
        (begin (hash-set! memo pos #t)
               (for-each (lambda (adj) (if (= (hash-ref tab adj (lambda () 9)) 9) 0 (visit adj)))
                         (make-adjs (car pos) (cadr pos))))))
    (visit pos)
    (hash-count memo))
  (define *tab* (get-tab name))
  (displayln (apply * (take (sort (map (lambda (p) (basin-size *tab* p)) (low-points *tab*)) >) 3))))

(sol1 "input9-1.txt")
(sol1 "input9-2.txt")
(sol2 "input9-1.txt")
(sol2 "input9-2.txt")

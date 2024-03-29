(define (in-bounds a bounds)
  (and (>= a (car bounds)) (<= a (cadr bounds))))

(define (find-xs bounds)
  (define (gets-to-target? x vx start-v)
    (cond ((in-bounds x bounds) start-v)
          ((= vx 0) #f)
          (else (gets-to-target? (+ x vx) (sub1 vx) start-v))))
  (filter-map (lambda (v) (gets-to-target? 0 v v))
              (inclusive-range 0 (cadr bounds))))

(define (pos-list init bounds-x bounds-y)
  (define (update p)
    (list (+ (car p) (caddr p)) (+ (cadr p) (cadddr p))
          (if (= (caddr p) 0) (caddr p) (sub1 (caddr p)))
          (sub1 (cadddr p))))
  (define (loop p lst)
    (cond ((or (> (car p) (cadr bounds-x)) (< (cadr p) (car  bounds-y)))    #f)
          ((and (in-bounds (car p) bounds-x) (in-bounds (cadr p) bounds-y)) (cons p lst))
          (else (loop (update p) (cons p lst)))))
  (loop (list 0 0 (car init) (cadr init)) '()))

(define (make-vels x starty endy)
  (for/list ([i (make-list (- endy starty) x)] [j (in-range starty endy)]) (list i j)))

(define (get-in-target bx by max-y)
  (define init-vels (apply append (map (lambda (x) (make-vels x (car by) max-y)) (find-xs bx))))
  (filter cadr (map (lambda (xy) (list xy (pos-list xy bx by))) init-vels)))

(define (list-max f lst) (apply max (map f lst)))
(define (sol1 bx by max-y)
  (displayln (list-max (lambda (x) (list-max cadr (cadr x)))
                       (get-in-target bx by max-y))))

(define (sol2 bx by max-y)
  (displayln (length (map car (get-in-target bx by max-y)))))

(sol1 '(20 30) '(-10 -5) 20)
(sol1 '(156 202) '(-110 -69) 500)
(sol2 '(20 30) '(-10 -5) 20)
(sol2 '(156 202) '(-110 -69) 500)

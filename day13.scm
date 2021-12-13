(define (parse name)
  (let* ([splitted (string-split (file->string name) "\n\n")]
         [folds (map (lambda (l) (list (string-ref l 11)
                                       (string->number (substring l 13 (string-length l)))))
                     (string-split (cadr splitted) "\n"))]
         [points (map (lambda (p) (map string->number (string-split p ",")))
                 (string-split (car splitted) "\n"))]
         [width  (apply max (map car  points))]
         [height (apply max (map cadr points))])
    (values points folds (list width height))))

(define (mat-print mat width height result-fn)
  (for ([i (in-range (add1 height))])
       (for ([j (in-range (add1 width))])
            (display (result-fn j i)))
       (newline)))
(define (points-print p w h) (mat-print p w h (lambda (j i) (if (member (list j i) p) #\# #\.))))

(define (fold points axis line size)
  (define index (if (char=? axis #\x) 0 1))
  (define-values (a b) (partition (lambda (x) (> (list-ref x index) line)) points))
  (define (fold-axis x) (- (* 2 line) x))
  (define new-points (remove-duplicates (append b (map (lambda (x) (list-update x index fold-axis)) a))))
  (define new-size (list-update size index (lambda (x) (- x line 1))))
  (list new-points new-size))

(define (sol1 name)
  (define-values (points folds size) (parse name))
  (displayln (length (car (fold points (caar folds) (cadr (car folds)) size)))))

(define (sol2 name)
  (define-values (points folds size) (parse name))
  (for-each (lambda (instr)
              (let* ([res (fold points (car instr) (cadr instr) size)])
                (set! points (car res))
                (set! size (cadr res))))
            folds)
  (points-print points (car size) (cadr size)))

(sol1 "input13-1.txt")
(sol1 "input13-2.txt")
(sol2 "input13-1.txt")
(sol2 "input13-2.txt")

(define null? (lambda (v) (eq? v '())))

(define cons (lambda (car cdr) (##make <pair> car cdr)))

(define car
  (lambda (ls)
    (if (pair? ls)
      (##slot-ref ls 0)
      (error "not a pair: " ls))))

(define cdr
  (lambda (ls)
    (if (pair? ls)
      (##slot-ref ls 1)
      (error "not a pair: " ls))))

(define list (lambda xs xs))

(define caar (lambda (ls) (car (car ls))))
(define cadr (lambda (ls) (car (cdr ls))))

(define for-each
  (lambda (f ls)
    (if (pair? ls)
      (begin
        (f (car ls))
        (for-each f (cdr ls)))
      (if (null? ls)
        (void)
        (error "for-each: improper list:" ls)))))

(define fold
  (lambda (f acc ls)
    (let* ((loop #f))
      (begin
        (set! loop (lambda (ls* acc)
                     (if (pair? ls*)
                       (loop (cdr ls*) (f (car ls*) acc))
                       (if (null? ls*)
                         acc
                         (error "fold: improper list:" ls)))))
        (loop ls acc)))))

(define length (lambda (ls) (fold (lambda (_ len) (fx+ len 1)) 0 ls)))

(define assq
  (lambda (obj ls)
    (if (pair? ls)
      (if (eq? (caar ls) obj)
        (car ls)
        (assq obj (cdr ls)))
      (if (null? ls)
        #f
        (error "memq: improper list: " ls)))))


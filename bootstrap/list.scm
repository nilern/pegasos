(define list (lambda xs xs))

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


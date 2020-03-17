(define list (lambda xs xs))

(define cadr (lambda (ls) (car (cdr ls))))

(define length
  (lambda (ls)
    (let* ((loop #f))
      (begin
        (set! loop (lambda (ls len)
                     (if (null? ls)
                       len
                       (loop (cdr ls) (fx+ len 1)))))
        (loop ls 0)))))

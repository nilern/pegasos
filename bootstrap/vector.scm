(define vector-length %length)

(define vector-ref %slot-ref)

(define vector-set! %slot-set!)

(define vector-map
  (lambda (f vec)
    (let* ((len (vector-length vec))
           (vec* (make-vector len))
           (loop #f))
      (begin
        (set! loop (lambda (i)
                     (if (fx<? i len)
                       (begin
                         (vector-set! vec* i (f (vector-ref vec i)))
                         (loop (fx+ i 1)))
                       vec*)))
        (loop 0)))))


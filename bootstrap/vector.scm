(define vector-length
  (lambda (vec)
    (if (vector? vec)
      (##object-length vec)
      (error "vector-length: not a vector"))))

(define vector-ref
  (lambda (vec i)
    (if (vector? vec)
      (##slot-ref vec i)
      (error "vector-ref: not a vector"))))

(define vector-set!
  (lambda (vec i v)
    (if (vector? vec)
      (##slot-set! vec i v)
      (error "vector-set!: not a vector"))))

(define vector-copy!
  (let* ((vector-copy-forward!
           (lambda (to at from start end)
             (let* ((copy! #f))
               (begin
                 (set! copy! (lambda (at start)
                               (if (fx< start end)
                                 (begin
                                   (vector-set! to at (vector-ref from start))
                                   (copy! (fx+ at 1) (fx+ start 1)))
                                 (void))))
                 (copy! at start)))))

         (vector-copy-backward!
           (lambda (to at from start end)
             (let* ((copy! #f))
               (begin
                 (set! copy! (lambda (at last)
                               (if (fx<= start last)
                                 (begin
                                   (vector-set! to at (vector-ref from last))
                                   (copy! (fx- at 1) (fx- start 1)))
                                 (void))))
                 (copy! (fx- (fx+ at (fx- end start)) 1) (fx- end 1)))))))
    (lambda (to at from start end)
      (if (not (eq? to from))
        (vector-copy-forward! to at from start end)
        (if (fx< at start)
          (vector-copy-forward! to at from start end)
          (vector-copy-backward! to at from start end))))))

(define vector-set
  (lambda (vec i v)
    (let* ((vec* (vector-copy vec)))
      (begin
        (vector-set! vec* i v)
        vec*))))

(define vector-insert
  (lambda (vec i v)
    (let* ((vec* (make-vector (fx+ (vector-length vec) 1))))
      (begin
        (vector-copy! vec* 0 vec 0 i)
        (vector-set! vec* i v)
        (vector-copy! vec* (fx+ i 1) vec i (vector-length vec))
        vec*))))

(define vector-delete
  (lambda (vec i)
    (let* ((vec* (make-vector (fx+ (vector-length vec) 1))))
      (begin
        (vector-copy! vec* 0 vec 0 i)
        (vector-copy! vec* i vec (fx+ i 1) (vector-length vec))
        vec*))))

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

(define vector-every
  (lambda (pred vec)
    (let* ((len (vector-length vec))
           (loop #f))
      (begin
        (set! loop (lambda (i res)
                     (if (fx<? i len)
                       (let* ((v (pred (vector-ref vec i))))
                         (if v
                           (loop (fx+ i 1) v)
                           #f))
                       res)))
        (loop 0 #t)))))


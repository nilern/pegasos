(define vector-length %length)

(define vector-ref %slot-ref)

(define vector-set! %slot-set!)

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


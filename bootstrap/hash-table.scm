(define <hash-table>
  (make-rtd '<hash-table> #((immutable equality) (immutable hash)
                            (mutable keys) (mutable values)
                            (mutable size))))

(define %hash-table-vacant (make-vector 1))

(define make-hash-table
  (let ((make (rtd-constructor <hash-table>)))
    (lambda (comparator)
      (let ((equality (comparator-equality-predicate comparator))
            (hash (comparator-hash-function comparator)))
        (make equality hash (make-vector 2 %hash-table-vacant) (make-vector 2) 0)))))

(define hash-table-equivalence-function (rtd-accessor <hash-table> 'equality))
(define hash-table-hash-function (rtd-accessor <hash-table> 'hash))
(define %hash-table-keys (rtd-accessor <hash-table> 'keys))
(define %hash-table-keys-set! (rtd-mutator <hash-table> 'keys))
(define %hash-table-values (rtd-accessor <hash-table> 'values))
(define %hash-table-values-set! (rtd-mutator <hash-table> 'values))
(define hash-table-size (rtd-accessor <hash-table> 'size))
(define %hash-table-size-set! (rtd-mutator <hash-table> 'size))

(define %hash-table-capacity (lambda (hash-table) (vector-length (%hash-table-keys hash-table))))

(define %hash-table-index-of
  (lambda (hash-table k)
    (let ((equality (hash-table-equivalence-function hash-table))
          (hash ((hash-table-hash-function hash-table) k))
          (keys (%hash-table-keys hash-table)))
      (let ((capacity (vector-length keys))
            (loop #f))
        (begin
          (set! loop (lambda (collisions)
                       (if (fx<? collisions capacity)
                         (let ((i (bitwise-and (fx+ hash collisions) (fx- capacity 1))))
                           (let ((k* (vector-ref keys i)))
                             (if (eq? k* %hash-table-vacant)
                               (values i #f)
                               (if (equality k* k)
                                 (values i #t)
                                 (loop (fx+ collisions 1))))))
                         (error "unreachable"))))
          (loop 0))))))

(define %hash-table-rehash-index-of
  (lambda (hash-table k)
    (let ((hash ((hash-table-hash-function hash-table) k))
          (keys (%hash-table-keys hash-table)))
      (let ((capacity (vector-length keys))
            (loop #f))
        (begin
          (set! loop (lambda (collisions)
                       (if (fx<? collisions capacity)
                         (let ((i (bitwise-and (fx+ hash collisions) (fx- capacity 1))))
                           (let ((k* (vector-ref keys i)))
                             (if (eq? k* %hash-table-vacant)
                               i
                               (loop (fx+ collisions 1)))))
                         (error "unreachable"))))
          (loop 0))))))

(define %hash-table-rehash-set!
  (lambda (hash-table k v)
    (let ((i (%hash-table-rehash-index-of hash-table k)))
      (begin
        (vector-set! (%hash-table-keys hash-table) i k)
        (vector-set! (%hash-table-values hash-table) i v)))))

(define %hash-table-rehash!
  (lambda (hash-table)
    (let ((keys (%hash-table-keys hash-table))
          (vals (%hash-table-values hash-table)))
      (let ((capacity (vector-length keys)))
        (let ((capacity* (arithmetic-shift capacity 1)))
          (let ((keys* (make-vector capacity* %hash-table-vacant))
                (vals* (make-vector capacity*))
                (loop #f))
            (begin
              (%hash-table-keys-set! hash-table keys*)
              (%hash-table-values-set! hash-table vals*)
              (set! loop (lambda (i)
                           (if (fx<? i capacity)
                             (let ((k (vector-ref keys i)))
                               (if (eq? k %hash-table-vacant)
                                 (loop (fx+ i 1))
                                 (begin
                                   (%hash-table-rehash-set! hash-table k (vector-ref vals i))
                                   (loop (fx+ i 1)))))
                             (void))))
              (loop 0))))))))

(define %hash-table-ensure-vacancy!
  (lambda (hash-table)
    (if (fx<? (hash-table-size hash-table)
              (arithmetic-shift (%hash-table-capacity hash-table) -1))
      (void)
      (%hash-table-rehash! hash-table))))

(define hash-table-ref
  (lambda (hash-table k failure)
    (call-with-values (lambda () (%hash-table-index-of hash-table k))
                      (lambda (i has-value)
                        (if has-value
                          (vector-ref (%hash-table-values hash-table) i)
                          (failure))))))

(define hash-table-set!
  (lambda (hash-table k v)
    (begin
      (%hash-table-ensure-vacancy! hash-table)
      (call-with-values (lambda () (%hash-table-index-of hash-table k))
                        (lambda (i has-value)
                          (begin
                            (if has-value
                              (void)
                              (%hash-table-size-set! hash-table (fx+ (hash-table-size hash-table) 1)))
                            (vector-set! (%hash-table-keys hash-table) i k)
                            (vector-set! (%hash-table-values hash-table) i v)))))))


(define make-rtd
  (lambda (name fieldspecs . args)
    (if (symbol? name)
      (let* ((parent (if (null? args)
                       #f
                       (car args)))
             (fields (vector-map (lambda (fieldspec)
                                   (if (symbol? fieldspec)
                                     (##make <field-descriptor> #t 8 fieldspec)
                                     (##make <field-descriptor>
                                       (eq? 'mutable (car fieldspec)) 8
                                       (cadr fieldspec))))
                                  fieldspecs)))
        (apply ##make-type #f #f name parent (vector->list fields)))
      (error "make-rtd: name is not a symbol" name))))

(define rtd-constructor
  (lambda (rtd)
    (let* ((arity (vector-length (##slot-ref rtd 2))))
      (lambda fieldvs ; OPTIMIZE
        (if (eq? (length fieldvs) arity)
          (apply ##record rtd fieldvs)
          #f)))))

(define rtd-predicate
  (lambda (rtd)
    (lambda (v)
      (instance? rtd v))))

(define rtd-field-index
  (lambda (rtd field-name)
    (let* ((fieldspecs (##slot-ref rtd 2))
           (len (vector-length fieldspecs))
           (loop #f))
      (begin
        (set! loop (lambda (i)
                     (if (fx<? i len)
                       (let* ((fieldspec (vector-ref fieldspecs i)))
                         (if (eq? (cadr fieldspec) field-name)
                           (cons i fieldspec)
                           (loop (fx+ i 1))))
                       #f)))
        (loop 0)))))

(define rtd-accessor
  (lambda (rtd field-name)
    (let* ((predicate? (rtd-predicate rtd))
           (i (fx+ (car (rtd-field-index rtd field-name)) 1)))
      (lambda (record)
        (if (predicate? record)
          (##slot-ref record i)
          (error "record field accessor: type error"))))))

(define rtd-mutator
  (lambda (rtd field-name)
    (let* ((predicate? (rtd-predicate rtd))
           (index-spec (rtd-field-index rtd field-name))
           (spec (cdr index-spec)))
      (if (eq? (car spec) 'mutable)
        (let* ((i (fx+ (car index-spec) 1)))
          (lambda (record v)
            (if (predicate? record)
              (##slot-set! record i v)
              (error "record field mutator: type error"))))
        (error "rtd-mutator: field is immutable")))))


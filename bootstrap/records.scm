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
    (let* ((arity (##flex-length rtd)))
      (lambda fieldvs ; OPTIMIZE
        (if (eq? (length fieldvs) arity)
          (apply ##make rtd fieldvs)
          #f)))))

(define rtd-predicate
  (lambda (rtd)
    (lambda (v)
      (instance? rtd v))))

(define rtd-field-index
  (lambda (rtd field-name)
    (let* ((field-count (##flex-length rtd))
           (loop #f))
      (begin
        (set! loop (lambda (i)
                     (if (fx<? i field-count)
                       (let* ((fieldspec (##flex-ref rtd i)))
                         (if (eq? (##slot-ref fieldspec 2) field-name)
                           (cons i fieldspec)
                           (loop (fx+ i 1))))
                       #f)))
        (loop 0)))))

(define rtd-accessor
  (lambda (rtd field-name)
    (let* ((predicate? (rtd-predicate rtd))
           (i (car (rtd-field-index rtd field-name))))
      (lambda (record)
        (if (predicate? record)
          (##slot-ref record i)
          (error "record field accessor: type error"))))))

(define rtd-mutator
  (lambda (rtd field-name)
    (let* ((predicate? (rtd-predicate rtd))
           (index-spec (rtd-field-index rtd field-name))
           (spec (cdr index-spec)))
      (if (##slot-ref spec 0)
        (let* ((i (car index-spec)))
          (lambda (record v)
            (if (predicate? record)
              (##slot-set! record i v)
              (error "record field mutator: type error"))))
        (error "rtd-mutator: field is immutable")))))


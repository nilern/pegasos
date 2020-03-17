(define <record-type> #f)

(define make-rtd
  (lambda (name fieldspecs . args)
    (let* ((parent (if (null? args)
                     #f
                     (car args)))
           (fields (vector-map (lambda (fieldspec)
                                 (if (symbol? fieldspec)
                                   (list 'mutable fieldspec)
                                   fieldspec))
                                fieldspecs)))
       (%record <record-type> parent fields name))))

(set! <record-type>
  (make-rtd '<record-type>
    #((immutable parent)
      (immutable fields)
      (immutable name))))
(%slot-set! <record-type> 0 <record-type>)

(define rtd-constructor
  (lambda (rtd)
    (let* ((arity (vector-length (%slot-ref rtd 2))))
      (lambda fieldvs
        (if (eq? (length fieldvs) arity)
          (apply %record rtd fieldvs)
          #f)))))

(define rtd-predicate
  (lambda (rtd)
    (lambda (v)
      (instance? rtd v))))

(define rtd-field-index
  (lambda (rtd field-name)
    (let* ((fieldspecs (%slot-ref rtd 2))
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
    (let* ((i (fx+ (car (rtd-field-index rtd field-name)) 1)))
      (lambda (record)
        (%slot-ref record i)))))

(define rtd-mutator
  (lambda (rtd field-name)
    (let* ((index-spec (rtd-field-index rtd field-name))
           (spec (cdr index-spec)))
      (if (eq? (car spec) 'mutable)
        (let* ((i (fx+ (car index-spec) 1)))
          (lambda (record v)
            (%slot-set! record i v)))
        #f))))


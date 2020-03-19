;;;; # Syntax Objects

(define syntax-e (lambda (s) (%slot-ref s 0)))
(define %syntax-scopes
  (lambda (s)
    (let* ((scopes (%slot-ref s 1)))
      (if scopes scopes (set-eq)))))
(define syntax-source (lambda (s) (%slot-ref s 2)))
(define syntax-line (lambda (s) (%slot-ref s 3)))
(define syntax-column (lambda (s) (%slot-ref s 4)))

(define identifier? syntax?)

(define datum->syntax
  (lambda (v)
    (if (syntax? v)
      v
      (if (symbol? v)
        (make-syntax v (set-eq) "datum->syntax" 1 1)
        (if (pair? v)
          (cons (datum->syntax (car v)) (datum->syntax (cdr v)))
          v)))))

(define syntax->datum
  (lambda (s)
    (if (syntax? s)
      (syntax-e s)
      (if (pair? s)
        (cons (syntax->datum (car s)) (syntax->datum (cdr s)))
        s))))

;;;; # Scopes

(define <scope> (make-rtd '<scope> #()))
(define scope (rtd-constructor <scope>))

(define adjust-scope
  (lambda (s sc f)
    (if (syntax? s)
      (make-syntax (syntax-e s) (f (%syntax-scopes s) sc)
                   (syntax-source s) (syntax-line s) (syntax-column s))
      (if (pair? s)
        (cons (adjust-scope (car s) sc f)
              (adjust-scope (cdr s) sc f))
        s))))

(define add-scope (lambda (s sc) (adjust-scope s sc set-adjoin)))

(define set-flip
  (lambda (set sc)
    (if (set-member set sc #f)
      (set-delete set sc) ; FIXME: `set-delete` unimplemented
      (set-adjoin set sc))))

(define flip-scope (lambda (s sc) (adjust-scope s sc set-flip)))

;;;; # Bindings

(define %all-bindings (make-hash-table (make-comparator (lambda () #t) eq? #f %identity-hash)))

(define add-binding! (lambda (id binding) (hash-table-set! %all-bindings id binding)))

(define find-all-matching-bindings
  (lambda (id)
    (hash-table-fold (lambda (id* _ acc)
                       (if (eq? (syntax-e id*) (syntax-e id))
                        (if (set<=? (%syntax-scopes id*) (%syntax-scopes id))
                          (cons id* acc)
                          acc)
                        acc))
                     '() %all-bindings)))

(define check-unambiguous
  (lambda (max-id candidate-ids)
    (for-each (lambda (candidate)
                (if (set<=? (%syntax-scopes candidate) (%syntax-scopes max-id))
                  #t
                  (error "ambiguous:" max-id)))
              candidate-ids)))

(define resolve
  (lambda (id)
    (let* ((candidate-ids (find-all-matching-bindings id)))
      (if (pair? candidate-ids)
        (let* ((max-id (fold (lambda (candidate current-max)
                               (if current-max
                                 (if (fx<? (set-size (%syntax-scopes current-max))
                                           (set-size (%syntax-scopes candidate)))
                                   candidate
                                   current-max)
                                 candidate))
                             #f candidate-ids)))
          (begin
            (check-unambiguous max-id candidate-ids)
            (hash-table-ref %all-bindings max-id (lambda () (error "unreachable")))))
        #f))))


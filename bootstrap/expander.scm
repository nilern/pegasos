;;;; # Syntax Objects

(define make-syntax ##make-syntax)
(define syntax-e (lambda (s) (##slot-ref s 0)))
(define %syntax-scopes
  (lambda (s)
    (let* ((scopes (##slot-ref s 1)))
      (if scopes scopes (set-eq)))))
(define syntax-source (lambda (s) (##slot-ref s 2)))
(define syntax-line (lambda (s) (##slot-ref s 3)))
(define syntax-column (lambda (s) (##slot-ref s 4)))

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

(define %all-bindings (make-hash-table (make-comparator (lambda () #t) eq? #f ##identity-hash)))

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

;;;; # Kernel

(define intrinsic?
  (let* ((crunch-byte (char->integer #\#)))
    (lambda (s)
      (let* ((name (syntax-e s)))
        (if (eq? (##flex-ref name 0) crunch-byte)
          (eq? (##flex-ref name 1) crunch-byte)
          #f)))))

(define core-scope (scope))

(define core-forms (set-eq 'lambda 'let-syntax 'quote 'syntax))

(set-for-each
  (lambda (form) (add-binding! (make-syntax form (set-eq core-scope) "#<special-form>" 1 1) form))
  core-forms)

(define introduce (lambda (s) (add-scope s core-scope)))

;;;; # Expansion Env

;;; OPTIMIZE: Something better than alists, e.g. HAMT

(define empty-env '())

(define variable (gensym 'variable))

(define env-extend (lambda (env binding v) (cons (cons binding v) env)))

(define env-lookup
  (lambda (env binding)
    (let* ((kv (assq binding env)))
      (if kv (car kv) kv))))

(define add-local-binding!
  (lambda (id)
    (let* ((binding (gensym (syntax-e id))))
      (add-binding! id binding)
      binding)))

;;;; # Expander Proper

(define expand
  (lambda (s . args) ; TODO: use `case-lambda`
    (let* ((env (if (pair? args) (car args) empty-env)))
      (if (identifier? s)
        (expand-identifier s env)
        (if (pair? s)
          (if (identifier? (car s))
            (expand-id-application s env)
            (expand-app s env))
          (error "bad syntax:" s))))))

(define expand-identifier
  (lambda (s env)
    (if (intrinsic? s)
      s
      (let* ((binding (resolve s)))
        (if binding
          (if (special-form? binding)
            (error "bad syntax:" s)
            (let* ((v (env-lookup (env binding))))
              (if (eq? v variable)
                s
                (if (not v)
                  (error "out of context:" s)
                  (error "bad syntax:" s)))))
          (error "unbound variable:" s))))))

(define expand-id-application
  (lambda (s env)
    (let* ((id (car s))
           (binding (resolve id)))
      (case binding
        ((lambda) (expand-lambda s env))
        ((let-syntax) (expand-let-syntax s env))
        ((quote) s)
        ((syntax) s)
        (else (let* ((v (env-lookup env binding)))
                (if (procedure? v)
                  (expand (apply-transformer v s) env)
                  (expand-app s env))))))))

(define apply-transformer
  (lambda (t s)
    (let* ((intro-scope (scope))
           (intro-s (add-scope s intro-scope))
           (transformed-s (t intro-s)))
      (flip-scope transformed-s intro-scope))))

(define expand-lambda
  (lambda (s env)
    (let* ((lambda-id (car s))
           (arg-id (caadr s))
           (body (caddr s))
           (sc (scope))
           (id (add-scope arg-id sc))
           (binding (add-local-binding! id))
           (body-env (env-extend env binding variable))
           (exp-body (expand (add-scope body sc) body-env)))
      (list lambda-id (list id) exp-body))))

(define expand-let-syntax
  (lambda (s env)
    (let* ((let-syntax-id (car s))
           (lhs-id (caaadr s))
           (rhs (cadaadr s))
           (body (caddr s))
           (sc (scope))
           (id (add-scope lhs-id sc))
           (binding (add-local-binding! id))
           (rhs-val (eval-for-syntax-binding rhs))
           (body-env (env-extend env binding rhs-val)))
      (expand (add-scope body sc) body-env))))

(define expand-app (lambda (s env) (map (lambda (s) (expand s env)) s)))

;;;;

(define eval-for-syntax-binding
  (lambda (rhs)
    (eval-compiled (compile (expand rhs empty-env)))))


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


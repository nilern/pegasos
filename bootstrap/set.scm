(define <hash-set> (make-rtd '<hash-set> #((immutable equality) (immutable hash)
                                           (immutable trie)
                                           (immutable size))))

(define %hash-set (rtd-constructor <hash-set>))

(define %hash-set-equality (rtd-accessor <hash-set> 'equality))
(define %hash-set-hash (rtd-accessor <hash-set> 'hash))
(define %hash-set-trie (rtd-accessor <hash-set> 'trie))
(define set-size (rtd-accessor <hash-set> 'size))

;; OPTIMIZE: Indexed field?
(define <bitmap-node> (make-rtd '<bitmap-node> #((immutable bitmap) (immutable nodes))))

(define %bitmap-node (rtd-constructor <bitmap-node>))
(define %bitmap-node? (rtd-predicate <bitmap-node>))

(define %bitmap-node-bitmap (rtd-accessor <bitmap-node> 'bitmap))
(define %bitmap-node-nodes (rtd-accessor <bitmap-node> 'nodes))

(define %bitmap-node-bits 5)
(define %bitmap-node-width (arithmetic-shift 1 %bitmap-node-bits))
(define %bitmap-node-mask (fx- %bitmap-node-width 1))

(define %bitmap-node-hash-part
  (lambda (hash shift)
    (bitwise-and (arithmetic-shift hash (fx- shift)) %bitmap-node-mask)))

(define %bitmap-node-bitpos
  (lambda (hash shift)
    (arithmetic-shift 1 (%bitmap-node-hash-part hash shift))))

(define %bitmap-node-bit-index
  (lambda (bitmap bit)
    (bit-count (bitwise-and bitmap (fx- bit 1)))))

(define <collision-node> (make-rtd '<collision-node> #((immutable hash) (immutable vals))))

(define %collision-node (rtd-constructor <collision-node>))
(define %collision-node? (rtd-predicate <collision-node>))

(define %empty-trie (%bitmap-node 0 #()))

(define set
  (lambda (comparator)
    (%hash-set (comparator-equality-predicate comparator) (comparator-hash-function comparator)
               %empty-trie 0)))

(define set-eq (lambda () (%hash-set eq? ##identity-hash %empty-trie 0)))

(define %hash-set-trie-member
  (lambda (equality trie element default hash shift)
    (if (%bitmap-node? trie)
      (let* ((bitmap (%bitmap-node-bitmap trie))
             (bit (%bitmap-node-bitpos hash shift)))
        (if (not (eq? (bitwise-and bitmap bit) 0))
          (let* ((i (%bitmap-node-bit-index bitmap bit))
                 (node (vector-ref (%bitmap-node-nodes trie) i)))
            (%hash-set-trie-member equality node element default hash (fx+ shift %bitmap-node-bits)))
          default))

      (if (%collision-node? trie)
        (%hash-set-collision-member equality trie element default)

        (if (equality element trie)
          trie
          default)))))

(define set-member
  (lambda (set element default)
    (%hash-set-trie-member (%hash-set-equality set) (%hash-set-trie set) element default
                           ((%hash-set-hash set) element) 0)))

(define %hash-set-trie-adjoin
  (lambda (size trie element hash shift)
    (if (%bitmap-node? trie)
      (let* ((bitmap (%bitmap-node-bitmap trie))
             (nodes (%bitmap-node-nodes trie))
             (bit (%bitmap-node-bitpos hash shift))
             (i (%bitmap-node-bit-index bitmap bit)))
        (if (not (eq? (bitwise-and bitmap bit) 0))
          (let* ((node (vector-ref (%bitmap-node-nodes trie) i)))
            (call-with-values (lambda () (%hash-set-trie-adjoin size node element hash (fx+ shift %bitmap-node-bits)))
                              (lambda (node size)
                                (values (%bitmap-node bitmap (vector-set nodes i node)) size))))
          (values (%bitmap-node (bitwise-ior bitmap bit) (vector-insert nodes i element))
                  (fx+ size 1))))

      (let* ((equality (%hash-set-equality element trie)))
        (if (%collision-node? trie)
          (%hash-set-collision-adjoin size trie element hash (fx+ shfit %bitmap-node-bits))

          (if (equality element trie)
            (values element size)
            (let* ((hash* (hash-function trie)))
              (values (if (eq? hash* hash)
                        (collision-node hash #(trie element))
                        (let* ((node %empty-trie))
                          (call-with-values (lambda () (%hash-set-trie-adjoin size node trie hash* shift))
                                            (lambda (node _)
                                              (call-with-values (lambda () (%hash-set-trie-adjoin size node element hash shift))
                                                                (lambda (node _) node))))))
                      (fx+ size 1)))))))))

(define set-adjoin
  (lambda (set element)
    (call-with-values (lambda ()
                        (%hash-set-trie-adjoin (set-size set) (%hash-set-trie set) element
                                               ((%hash-set-hash set) element) 0))
                      (lambda (trie size)
                        (%hash-set (%hash-set-equality set) (%hash-set-hash set)
                                   trie size)))))

(define %hash-set-trie-delete
  (lambda (equality trie element hash shift)
    (if (%bitmap-node? trie)
      (let* ((bitmap (%bitmap-node-bitmap trie))
             (nodes (%bitmap-node-nodes trie))
             (bit (%bitmap-node-bitpos hash shift))
             (i (%bitmap-node-bit-index bitmap bit)))
        (if (not (eq? (bitwise-and bitmap bit) 0))
          (let* ((node (vector-ref (%bitmap-node-nodes trie) i)))
            (call-with-values (lambda () (%hash-set-trie-delete equality node element hash (fx+ shift %bitmap-node-bits)))
                              (lambda (node change)
                                (if (eq? change 'unchanged)
                                  (values trie change)
                                  (if (eq? change 'shrank)
                                    (values (%bitmap-node bitmap (vector-set nodes i node)) change)
                                    (values (%bitmap-node (bitwise-xor bitmap bit) (vector-delete nodes i)) ; 'gone
                                            'shrank))))))
          (values trie 'unchanged)))

      (if (%collision-node? trie)
        (%hash-set-collision-delete equality trie element hash shift)

        (if (equality element trie)
          (values trie 'gone)
          (values trie 'unchanged))))))

(define set-delete
  (lambda (set element)
    (call-with-values (lambda ()
                        (%hash-set-trie-delete (%hash-set-equality set) (%hash-set-trie set) element
                                               ((%hash-set-hash set) element) 0))
                      (lambda (trie change)
                        (if (eq? change 'unchanged)
                          set
                          (if (eq? change 'shrank)
                             (%hash-set (%hash-set-equality set) (%hash-set-hash set) trie (fx- (set-size set) 1))
                             (%hash-set (%hash-set-equality set) (%hash-set-hash set) %empty-trie 0))))))) ; 'gone

(define set<=?
  (lambda (set1 set2)
    (set-every? (lambda (elem) (set-member set2 elem #f)) set1)))

(define %hash-set-trie-every
  (lambda (pred trie)
    (if (%bitmap-node? trie)
      (vector-every (lambda (node) (%hash-set-trie-every pred node))
                    (%bitmap-node-nodes trie))
      (if (%collision-node? trie)
        (vector-every pred (%collision-node-vals trie))
        (pred trie)))))

(define set-every?
  (lambda (pred set)
    (if (%hash-set-trie-every pred (%hash-set-trie set)) #t #f)))


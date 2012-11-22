#lang racket/base

;; rbtree implementation of the token-tree% interface.

(require (prefix-in rb: (submod "red-black.rkt" uncontracted))
         racket/class)


(provide token-tree% 
         insert-first! 
         insert-last!
         insert-last-spec!
         node? node-token-length node-token-data 
         node-left-subtree-length node-left node-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-local-member-name
  get-rb
  set-rb!
  get-focus
  set-focus!)


(define token-tree%
  (class object%
    (init (length #f) (data #f))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state initialization
    (define rb (rb:new-tree))  ;; rb is an instance of rb:tree.
    (define focus rb:nil)      ;; focus is an instance of rb:node.
    (when length
      (rb:insert-last/data! rb data length))
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; private methods:
    (define/public (get-rb) 
      rb)
    (define/public (set-rb! new-rb) 
      (set! rb new-rb))
    (define/public (get-focus) 
      focus)
    (define/public (set-focus! new-focus) 
      (set! focus new-focus))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; public methods:

    ;; reset-tree: -> void
    ;; Empty the contents of the tree.
    (define/public (reset-tree)
      (set! rb (rb:new-tree))
      (set! focus rb:nil))

    (define/public (get-root)
      focus)
    
    (define/public (is-empty?)
      (rb:nil-node? focus))

    (define/public (get-root-data)
      (cond
        [(rb:nil-node? focus)
         #f]
        [else
         (rb:node-data focus)]))

    (define/public (get-root-start-position)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         (rb:position focus)]))

    (define/public (get-root-end-position)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         (+ (rb:position focus) (rb:node-self-width focus))]))
 
    (define/public (add-to-root-length inc)
      (unless (rb:nil-node? focus)
        (rb:update-node-self-width! focus (+ (rb:node-self-width focus) inc))))

    (define/public (search! key-position)
      (unless (rb:nil-node? focus)
        (set-focus! (rb:search rb key-position))))

    (define/public (search-max!)
      (unless (rb:nil-node? focus)
        (set-focus! (rb:tree-last rb))))
    
    (define/public (search-min!)
      (unless (rb:nil-node? focus)
        (set-focus! (rb:tree-first rb))))

    (define/public (remove-root!)
      (unless (rb:nil-node? focus)
        (define node-to-delete focus)
        (define pred (rb:predecessor focus))
        (cond [(rb:nil-node? pred)
               (set-focus! pred)]
              [else
               (set-focus! (rb:successor focus))])
        (rb:delete! rb node-to-delete)))
      

    (define/public (split/data pos)
      'fixme)

    (define/public (split pos)
      'fixme)
   
    (define/public (split-after)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-last! left focus)
         (set! focus rb:nil)
         (values left right)]))
        

    (define/public (split-before)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-first! right focus)
         (set! focus rb:nil)
         (values left right)]))


    (define/public (to-list)
      (cond
        [(rb:nil-node? focus) '()]
        [else
         (reverse 
          (rb:tree-fold-inorder rb
                                (lambda (n acc)
                                  (cons (vector (rb:node-self-width n)
                                                (node-left-subtree-length n)
                                                (rb:node-data n))
                                        acc))
                                '()))]))

    (define/public (for-each f)
      (cond
        [(rb:nil-node? focus)
         (void)]
        [else
         (rb:tree-fold-inorder rb
                               (lambda (n acc)
                                 (f acc 
                                    (rb:node-self-width n)
                                    (rb:node-data n))
                                 (+ acc (rb:node-self-width n)))
                               0)]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; insert-first!: token-tree% * token-tree% -> void
;; insert tree2 into tree1 as the first thing.
;;
;; Effects:
;;
;; 1.  tree1 will contain the contents of tree2 + tree1
;; 2.  tree2 will be reset to the empty tree.
;;
;; I'm not exactly sure if the behavior of where the tree is focused
;; is something defined.
(define (insert-first! tree1 tree2)
  (define-values (rb1 rb2)
    (values (send tree1 get-rb) (send tree2 get-rb)))
  (define rb-joined (rb:join! rb2 rb1))
  (send tree1 set-rb rb-joined)
  (send tree1 set-focus (rb:tree-root rb-joined))
  (send tree2 reset-tree))


;; insert-last!: token-tree% * token-tree%  -> void
;; insert tree2 into tree1 as the last thing.
;;
;; Effects:
;;
;; 1.  tree1 will contain the contents of tree1 + tree2
;; 2.  tree2 will be reset to the empty tree.
;;
;; I'm not exactly sure if the behavior of where the tree is focused
;; is something defined.
(define (insert-last! tree1 tree2)
  (define-values (rb1 rb2)
    (values (send tree1 get-rb) (send tree2 get-rb)))
  (define rb-joined (rb:join! rb1 rb2))
  (send tree1 set-rb rb-joined)
  (send tree1 set-focus (rb:tree-root rb-joined))
  (send tree2 reset-tree))



;; insert-last-spec!: tree natural any -> void
;; Inserts content at the end of the tree.
(define (insert-last-spec! tree length data)
  (rb:insert-last/data! (send tree get-rb) data length))




(define node? 
  (procedure-rename rb:node? 'node?))
(define node-token-data
  (procedure-rename rb:node-data 'node-token-data))
(define node-token-length
  (procedure-rename rb:node-self-width 'node-token-length))
(define (node-left-subtree-length n)
  (rb:node-subtree-width (rb:node-left n)))
(define node-left (procedure-rename rb:node-left 'node-left))
(define node-right (procedure-rename rb:node-right 'node-right))

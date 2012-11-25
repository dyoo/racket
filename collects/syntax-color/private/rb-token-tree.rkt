#lang racket/base

;; rbtree implementation of the token-tree% interface.
;;
;; We have to adapt a few things:
;;
;;     * rb-trees don't move around their root on search, so we need
;;       to keep a separate "focus".
;;
;;     * We use rb:nil, but the original client uses #f to indicate
;;     empty trees.

;; For speed, we use the uncontracted forms in red-black.rkt.
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
      (rb:insert-last/data! rb data length)
      (set! focus (rb:tree-root rb)))
    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (define (rb->token-tree an-rb)
      (define t (new token-tree%))
      (send t set-rb! an-rb)
      (send t set-focus! (rb:tree-root an-rb)))


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
      (nil->false focus))

    
    (define/public (is-empty?)
      (rb:nil-node? focus))

    (define/public (get-root-length)
      (cond
        [(rb:nil-node? focus)
         0]
        [else
         (rb:node-self-width focus)]))

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
      


    ;; split/data: natural -> (values natural natural token-tree% token-tree% boolean)
    ;; Splits the tree into 2 trees, invalidating our own to nil.
    ;;
    ;; The first two returned values represent the start and end
    ;; position of the token(s) at pos.  The next two values represent
    ;; the tokens before pos and after pos, not including any tokens
    ;; adjacent to pos.
    ;;
    ;; Thus if pos is on a token boundary, 2 tokens will be dropped.
    ;;
    ;; In this case, the start will be for the first dropped
    ;; token and the stop will be for the second.
    ;;
    ;; The last value is the data at the searched position.
    (define/public (split/data pos)
      (cond
        [(rb:nil-node? focus)
         (values 0 0 (new token-tree%) (new token-tree%) #f)]
        [else

         ;; We have a few cases to check for:
         ;; Is the pivot on the edge boundary of the first or last tokens?
         ;; Is the pivot on the boundary between two tokens?
         (cond

          ;; Case 1.
          ;; At the start-edge of the first token?
          [(= pos 0)
           ;; If so, just delete the first token.
           (define first-token (rb:tree-first rb))
           (rb:delete! rb first-token)
           (define right-tree (rb->token-tree rb))
           (set-focus! rb:nil)
           (values 0 (rb:node-self-width first-token)
                   (new token-tree%) right-tree 
                   (rb:node-data first-token))]

          ;; Case 2.
          ;; At the end-edge of the last token?
          [(= pos (rb:node-subtree-width (rb:tree-root rb)))
           ;; Symmetric case.
           (define last-token (rb:tree-last rb))
           (rb:delete! rb last-token)
           (define left-tree (rb->token-tree rb))
           (set-focus! rb:nil)
           (values (- (rb:node-self-width last-token) pos) pos
                   left-tree (new token-tree%) 
                   (rb:node-data last-token))]

          [else
           ;; Otherwise, pos is either outside or somewhere inside the
           ;; range.
           (define-values (pivot-node residue) (rb:search/residual rb pos))
           (cond
            ;; If we're outside, just return the nil case:
            [(rb:nil-node? pivot-node)
             (values 0 0 (new token-tree%) (new token-tree%) #f)]

            ;; If the residue after searching is zero, then we're right
            ;; on the boundary between two tokens, and must delete both.
            [(= residue 0)
             (define-values (left right) (rb:split! rb pivot-node))

             ;; We know the left is non-empty, since otherwise we would
             ;; have hit case 1.
             (define left-last (rb:tree-last left))
             (rb:delete! left left-last)
             (values (- pos (rb:node-self-width left-last))
                     (+ pos (rb:node-self-width pivot-node))
                     (rb->token-tree left)
                     (rb->token-tree right)
                     (rb:node-data pivot-node))]

            [else
             ;; Otherwise, the position is inside just one token.
             (define start-pos (- pos residue))
             (define end-pos (+ start-pos (rb:node-self-width pivot-node)))
             (define-values (left right) (rb:split! rb pivot-node))
             (values start-pos end-pos 
                     (rb->token-tree left)
                     (rb->token-tree right)
                     (rb:node-data pivot-node))])])]))
    


    (define/public (split pos)
      (define-values (start-pos end-pos left-tree right-tree data)
        (split/data pos))
      (values start-pos end-pos left-tree right-tree))

   
    (define/public (split-after)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-last! left focus)
         (set! focus rb:nil)
         (values (rb->token-tree left) (rb->token-tree right))]))
        

    (define/public (split-before)
      (cond
        [(rb:nil-node? focus)
         (values (new token-tree%) (new token-tree%))]
        [else
         (define-values (left right) (rb:split! rb focus))
         (rb:insert-first! right focus)
         (set! focus rb:nil)
         (values (rb->token-tree left) (rb->token-tree right))]))


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
  (send tree1 set-rb! rb-joined)
  (send tree1 set-focus! (rb:tree-root rb-joined))
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
  (send tree1 set-rb! rb-joined)
  (send tree1 set-focus! (rb:tree-root rb-joined))
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

(define (node-left n)
  (cond [(eq? n #f) 
         #f]
        [else
         (nil->false (rb:node-left n))]))

(define (node-right n)
  (cond [(eq? n #f) 
         #f]
        [else
         (nil->false (rb:node-right n))]))

(define-syntax-rule (nil->false n)
  (if (eq? n rb:nil)
      #f
      n))

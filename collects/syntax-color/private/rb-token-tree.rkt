#lang racket/base

;; rbtree implementation of the token-tree% interface.

(require (prefix-in rb: "red-black.rkt")
         racket/class)


(provide token-tree% 
         insert-first! 
         insert-last!
         insert-last-spec!
         node? node-token-length node-token-data 
         node-left-subtree-length node-left node-right
         )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-local-member-name
  get-rb
  set-rb!
  get-focus
  set-focus!)


(define token-tree%
  (class object%
    (init (length #f) (data #f))

    ;;;;;
    ;; state initialization
    (define rb (rb:new-tree))  ;; rb is an instance of rb:tree.
    (define focus rb:nil)      ;; focus is an instance of rb:node.
    (when length
      (rb:insert-last/data! rb data length))
    (super-new)
    ;;;;;


    ;; methods:
    (define/public (get-rb) rb)
    (define/public (set-rb! new-rb) (set! rb new-rb))
    (define/public (get-focus) focus)
    (define/public (set-focus! new-focus) (set! focus new-focus))

    ;; reset-tree: -> void
    ;; Empty the contents of the tree.
    (define/public (reset-tree)
      (set! rb (rb:new-tree))
      (set! focus rb:nil))))

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

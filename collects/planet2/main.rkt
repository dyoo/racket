#lang racket/base

(provide install
         update
         remove
         show
         config
         create)

(require compiler/cm)


(define-syntax-rule (define-cmds-with-compiler-hack id ...)
  (begin
    (define id
      (lambda args
        (parameterize ([current-namespace (make-base-namespace)])
          (let ([cmd (dynamic-require 'planet2/main-impl 'id)]
                [msfh (dynamic-require 'compiler/cm 'manager-skip-file-handler)])
            
            (parameterize ([msfh (manager-skip-file-handler)]
                           [use-compiled-file-paths (list (string->path "compiled"))])
              (apply cmd args))))))
    ...))


(define-cmds-with-compiler-hack install update remove show config create)


(module* main racket/base
  (require compiler/cm)
  (parameterize ([current-namespace (make-base-namespace)])
    (let ([msfh (dynamic-require 'compiler/cm 'manager-skip-file-handler)])
      (parameterize ([msfh (manager-skip-file-handler)]
                     [use-compiled-file-paths (list (string->path "compiled"))])
        (dynamic-require '(submod planet2/main-impl main) #f)))))
#lang racket/base

(provide time-acc)

;; a little macro to help me measure how much time we spend in
;; the body of an expression.
(define-syntax-rule (time-acc id body ...)
  (let ([start (current-inexact-milliseconds)])
    (call-with-values (lambda ()
                        (let () body ...))
      (lambda vals
        (let ([stop (current-inexact-milliseconds)])
          (set! id (+ id (- stop start)))
          (apply values vals))))))

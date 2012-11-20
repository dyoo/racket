#lang racket/base

(provide time-acc total-time reset!)

(define total-time 0)

(define in-time-acc? #f)

(define (increment! time)
  (set! total-time (+ total-time time)))

(define (reset!)
  (set! total-time 0)
  (set! in-time-acc? #f))

(define (inside!)
  (set! in-time-acc? #t))

(define (outside!)
  (set! in-time-acc? #f))

;; a little macro to help me measure how much time we spend in
;; the body of an expression.
(define-syntax-rule (time-acc body ...)
  (if in-time-acc?
      (let () body ...)
      (let ([start (current-inexact-milliseconds)])
        (inside!)
        (call-with-values (lambda ()
                            (let () body ...))
          (lambda vals
            (let ([stop (current-inexact-milliseconds)])
              (outside!)
              (increment! (- stop start))
              (apply values vals)))))))

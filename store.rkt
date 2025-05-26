#lang racket

(require "datatypes.rkt")
(require (only-in (lib "eopl.ss" "eopl") eopl:error cases))

(define (report-invalid-reference) (eopl:error 'invalid-reference "\n\tillegal reference to memory!"))

(define the-store 'uninitialized)

(define empty-store
    (lambda ()
        '()
    )
)

(define get-store 
    (lambda ()
        the-store
    )
)

(define initialize-store! (lambda () (set! the-store (empty-store))))


(define newref 
    (lambda (val) 
        (let 
            (
                (next-ref (length the-store))
            ) 
            (set! the-store (append the-store (list val))) 
            next-ref
        )
    )
)

(define deref 
    (lambda (ref) 
        (list-ref the-store ref)
    )
)

(define setref-in-store
    (lambda (ref val store1)
        (cond
            ((null? store1)
                (report-invalid-reference ref store1)
            )
            ((equal? ref 0)
                (cons val (cdr store1))
            )
            (else
                (cons (car store1) (setref-in-store (- ref 1) val (cdr store1)))
            )
        )
    )
)

(define setref!
    (lambda (ref val)
        (set! the-store (setref-in-store ref val the-store))
    )
)

(provide (all-defined-out))
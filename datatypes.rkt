#lang racket

(require (lib "eopl.ss" "eopl"))

(define (report-expval-extractor-error! type) (eopl:error 'invalid-value "invalid value cast to ~s" type))

(define reference?
    (lambda (var)
        (integer? var)
    )
)

(define-datatype environment environment?
  (empty-environment)
  (extend-environment (var string?) (val reference?) (env environment?)))

(define expval->num
 (lambda (val) (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error! "number")))))

(define expval->bool
 (lambda (val) (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error! "boolean")))))

(define expval->list
  (lambda (val) (cases expval val
    (list-val (vals) vals)
    (else (report-expval-extractor-error! "list")))))

(define expval->string
 (lambda (val) (cases expval val
    (string-val (string) string)
    (else (report-expval-extractor-error! "string")))))

(define-datatype program program?
 (a-program (exprs (list-of expression?))))

(define-datatype expression expression?
 (const-exp (num number?))
 (bool-exp (b boolean?))
 (string-exp (string string?))
 (add-exp (exp1 expression?) (exp2 expression?))
 (sub-exp (exp1 expression?) (exp2 expression?))
 (mult-exp (exp1 expression?) (exp2 expression?))
 (div-exp (exp1 expression?) (exp2 expression?))
 (var-exp (var string?))
 (assign-exp (var string?) (exp1 expression?))
 (print-exp (var string?))
 (greater-exp (exp1 expression?) (exp2 expression?))
 (greater-equal-exp (exp1 expression?) (exp2 expression?))
 (less-exp (exp1 expression?) (exp2 expression?))
 (less-equal-exp (exp1 expression?) (exp2 expression?))
 (equal-exp (exp1 expression?) (exp2 expression?))
 (not-equal-exp (exp1 expression?) (exp2 expression?))
 (and-exp (exp1 expression?) (exp2 expression?))
 (or-exp (exp1 expression?) (exp2 expression?))
 (not-exp (exp1 expression?))
 (list-exp (elements (list-of expression?)))
 (index-exp (var string?) (exp1 expression?))
 (if-exp (condition expression?) (then-branch expression?) (else-branch (maybe expression?)))
 (block-exp (exprs (list-of expression?)))
 (while-exp (condition expression?) (body expression?))
 (for-exp (init expression?) (condition expression?) (update expression?) (body expression?))
 (func-exp (name string?) (params (list-of expression?)) (body expression?))
 (call-exp (name string?) (args (list-of expression?)))
 (param-exp (name string?) (type string?))
 (return-exp (exp expression?))
 (empty-exp)
)

(define-datatype expval expval?
 (num-val (num number?))
 (bool-val (bool boolean?))
 (list-val (vals (list-of expval?)))
 (string-val (string string?))
 (closure-val (params (list-of string?)) (body expression?) (env environment?))
 )

(define num-val?
    (lambda (val)
        (cases expval val
        (num-val (num)
            #t
        )
        (else
            #f
        )
        )
    )
)

(define bool-val?
    (lambda (val)
        (cases expval val
        (bool-val (bool)
            #t
        )
        (else
            #f
        )
        )
    )
)

(define string-val?
    (lambda (val)
        (cases expval val
        (string-val (string)
            #t
        )
        (else
            #f
        )
        )
    )
)

(provide (all-defined-out))

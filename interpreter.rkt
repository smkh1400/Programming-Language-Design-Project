#lang racket


(require "lexer.rkt")
(require "parser.rkt")
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")
(require "store.rkt")


(define parse-tree->ast 
    (lambda (node)
        (match node
            ((list 'Program decls)
            (a-program (map parse-tree->ast decls)))

            ((list 'VarDecl var type expr)
            (assign-exp var (parse-tree->ast expr)))

            ((list "+" left right)
            (add-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "-" left right)
            (sub-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "*" left right)
            (mult-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "/" left right)
            (div-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list ">" left right)
            (greater-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list ">=" left right)
            (greater-equal-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "<" left right)
            (less-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "<=" left right)
            (less-equal-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "==" left right)
            (equal-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "!=" left right)
            (not-equal-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "&&" left right)
            (and-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "||" left right)
            (or-exp (parse-tree->ast left) (parse-tree->ast right)))

            ((list "!" exp)
            (not-exp (parse-tree->ast exp)))

            ((list 'Assign var expr)
            (assign-exp var (parse-tree->ast expr)))

            ((list 'Print var)
            (print-exp var))

            ((? string? v)
            (var-exp v))

            ((? number? n)
            (const-exp n))

            ((? boolean? b)
            (bool-exp b))

            ((list 'StringLiteral s)
            (string-exp s))

            ((list 'List elements)
            (list-exp (map parse-tree->ast elements)))

            ((list 'Index var index)
            (index-exp var (parse-tree->ast index)))

            (else
            (error "Unknown parse tree node:" node))
        )
    )
)

(define input-program (open-input-file "test.prog"))
(define tokens (lex-all input-program))
(define token-generator (make-token-generator tokens))
(define parse-result (parse-full token-generator))
(close-input-port input-program)

(displayln parse-result)

(define ast (parse-tree->ast parse-result))

(displayln ast)

(define value-of-program  ;;;TODO: there is a mysterious problem with prog and it magically is put inside a list where is should not and I don't know why??????
    (lambda prog
        (begin
            (initialize-store!)
            (initialize-env!)
            (cases program (car prog) ;;;here
                (a-program (exprs)
                    (value-of-sequence exprs)
                )
            )
        )
    )
)

(define value-of-sequence
    (lambda (exprs)
        (cond
            ((null? exprs) (eopl:error 'value-of-sequence "Empty program"))
            ((null? (cdr exprs)) (value-of (car exprs)))
            (else
                (begin
                    (value-of (car exprs))
                    (value-of-sequence (cdr exprs))
                )
            )
        )
    )
)

(define value-of
    (lambda (exp)
        (cases expression exp
            (const-exp (num)
                (num-val num)
            )
            (bool-exp (b)
                (bool-val b)
            )
            (string-exp (s)
                (string-val s)
            )
            (add-exp (exp1 exp2)
                (num-val
                    (+
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (sub-exp (exp1 exp2)
                (num-val
                    (-
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (mult-exp (exp1 exp2)
                (num-val
                    (*
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (div-exp (exp1 exp2)
                (num-val
                    (exact->inexact
                        (/
                            (expval->num (value-of exp1))
                            (expval->num (value-of exp2))
                        )
                    )
                )
            )
            (greater-exp (exp1 exp2)
                (bool-val
                    (>
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (greater-equal-exp (exp1 exp2)
                (bool-val
                    (>=
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (less-exp (exp1 exp2)
                (bool-val
                    (<
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (less-equal-exp (exp1 exp2)
                (bool-val
                    (<=
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (equal-exp (exp1 exp2)
                (bool-val
                    (equal?
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (not-equal-exp (exp1 exp2)
                (bool-val
                    (not
                        (equal?
                            (expval->num (value-of exp1))
                            (expval->num (value-of exp2))
                        )
                    )
                )
            )
            (and-exp (exp1 exp2)
                (bool-val
                    (and
                        (expval->bool (value-of exp1))
                        (expval->bool (value-of exp2))
                    )
                )
            )
            (or-exp (exp1 exp2)
                (bool-val
                    (or
                        (expval->bool (value-of exp1))
                        (expval->bool (value-of exp2))
                    )
                )
            )
            (not-exp (exp1)
                (bool-val
                    (not
                        (expval->bool (value-of exp1))
                    )
                )
            )
            (var-exp (var)
                (deref (apply-env var (get-env)))
            )
            (list-exp (exprs)
                (let
                    (
                        (vals (map (lambda (e) (value-of e)) exprs))
                    )
                    (list-val vals)
                )
            )
            (index-exp (var exp1)
                (let
                    (
                        (array-val (expval->list (deref (apply-env var (get-env)))))
                        (idx (expval->num (value-of exp1)))
                    )
                    (list-ref array-val idx)
                )
            )
            (assign-exp (var exp1)
                (let
                    (
                        (val (value-of exp1))
                        (ref (apply-env var (get-env)))
                    )
                    (if ref
                        (setref! ref val)
                        (let
                            (
                                (new-ref (newref val))
                            )
                            (set-env! (extend-env var new-ref (get-env)))
                        )
                    )
                )
            )
            (print-exp (var)
                (let
                    (
                        (val (deref (apply-env var (get-env))))
                    )
                    (cond
                        ((num-val? val) (displayln (expval->num val)))
                        ((bool-val? val) (displayln (expval->bool val)))
                        ((string-val? val) (displayln (expval->string val)))
                        (else (error "Unkown value type in print" val))
                    )
                )
            )
        )
    )
)

(value-of-program ast)
#lang racket


(require (lib "eopl.ss" "eopl"))
(require "lexer.rkt")
(require "parser.rkt")
(require "datatypes.rkt")
(require "environment.rkt")
(require "store.rkt")

(define custom-string-ref
    (lambda (str index)
        (if 
            (or 
                (< index 0) 
                (>= index (string-length str)))
            ""
            (string (string-ref str index)))))

(define custom-list-ref
    (lambda (lst index)
        (if 
            (or 
                (< index 0) 
                (>= index (length lst)))
            (string-val "")
            (list-ref lst index))))

(define custom-string-set!
  (lambda (str index char-val)
    (if (or (< index 0) (>= index (string-length str)))
        str  ; Return original string if out of bounds
        (let ([new-str (string-copy str)])
          (string-set! new-str index char-val)
          new-str))))

(define custom-list-set!
  (lambda (lst index value)
    (if (or (< index 0) (>= index (length lst)))
        lst  ; Return original list if out of bounds
        (append (take lst index)      ; elements before index
                (list value)          ; new value
                (drop lst (+ index 1))))))  ; elements after index


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

            ((list "%" left right)
            (modulo-exp (parse-tree->ast left) (parse-tree->ast right)))

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

            ((list "-" exp)
            (minus-exp (parse-tree->ast exp)))

            ((list 'Assign var expr)
            (assign-exp var (parse-tree->ast expr)))

            ((list 'AssignIndex var index expr)
            (assign-index-exp var (parse-tree->ast index) (parse-tree->ast expr)))

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

            ((list 'If condition then-branch else-branch)
                (if-exp (parse-tree->ast condition) 
                (parse-tree->ast then-branch)
                (if (equal? else-branch 'None)
                    (empty-exp)
                    (parse-tree->ast else-branch))
                )
            )

            ((list 'Block exprs)
            (block-exp (map parse-tree->ast exprs)))

            ((list 'While condition body)
            (while-exp (parse-tree->ast condition) (parse-tree->ast body)))

            ((list 'For init condition update body)
            (for-exp (parse-tree->ast init) (parse-tree->ast condition) (parse-tree->ast update) (parse-tree->ast body)))

            ((list 'FuncDecl name params return-type body)
            (func-exp name (map parse-tree->ast params) (parse-tree->ast body)))

            ((list 'Call name args)
            (call-exp name (map parse-tree->ast args)))

            ((list 'Param name (list 'Type type-str))
            (param-exp name type-str))

            ((list 'Return expr)
            (return-exp (parse-tree->ast expr)))

            (else
            (error "Unknown parse tree node:" node))
        )
    )
)

(define input-program (open-input-file "q2.prog"))
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

;;; (define value-of-sequence
;;;     (lambda (exprs)
;;;         (cond
;;;             ((null? exprs) (eopl:error 'value-of-sequence "Empty program"))
;;;             ((null? (cdr exprs)) (value-of (car exprs)))
;;;             (else
;;;                 (begin
;;;                     (value-of (car exprs))
;;;                     (value-of-sequence (cdr exprs))
;;;                 )
;;;             )
;;;         )
;;;     )
;;; )

(define value-of-sequence
    (lambda (exprs)
        (cond
            ((null? exprs) (eopl:error 'value-of-sequence "Empty program"))
            (else 
                (let ((res (value-of (car exprs))))
                    (if (return-signal? res)
                        res
                        (if (null? (cdr exprs))
                            res
                            (value-of-sequence (cdr exprs))
                        )
                    )
                )
            )
        )
    )
)

(struct return-signal (value))

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
                    (quotient
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
                    )
                )
            )
            (modulo-exp (exp1 exp2)
                (num-val
                    (modulo
                        (expval->num (value-of exp1))
                        (expval->num (value-of exp2))
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
                    (let
                        (
                            (val1 (value-of exp1))
                            (val2 (value-of exp2))
                        )
                        (cond
                            ((and (num-val? val1) (num-val? val2)) 
                                (equal?
                                    (expval->num val1)
                                    (expval->num val2)
                                )
                            )
                            ((and (string-val? val1) (string-val? val2))
                                (equal?
                                    (expval->string val1)
                                    (expval->string val2)
                                )
                            )
                            (else
                                #f
                            )
                        )
                    )
                )
            )
            (not-equal-exp (exp1 exp2)
                (bool-val
                    (let
                        (
                            (val1 (value-of exp1))
                            (val2 (value-of exp2))
                        )
                        (cond
                            ((and (num-val? val1) (num-val? val2)) 
                                (not
                                    (equal?
                                        (expval->num val1)
                                        (expval->num val2)
                                    )
                                )
                            )
                            ((and (string-val? val1) (string-val? val2))
                                (not
                                    (equal?
                                        (expval->string val1)
                                        (expval->string val2)
                                    )
                                )
                            )
                            (else
                                #t
                            )
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
            (minus-exp (exp1)
                (num-val
                    (- (expval->num (value-of exp1)))
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
                        (val (deref (apply-env var (get-env))))
                        (idx (expval->num (value-of exp1)))
                    )
                    (cond
                        ((string-val? val) (string-val (custom-string-ref (expval->string val) idx)))
                        ((list-val? val) (custom-list-ref (expval->list val) idx))
                        (else (error "Unknown value type in index-exp" val))
                    )
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
            (assign-index-exp (var index-exp value-exp)
                (let* 
                    (
                        (ref (apply-env var (get-env)))
                        (current-val (deref ref))
                        (index (expval->num (value-of index-exp)))
                        (value (value-of value-exp))
                    )
                    (if ref
                        (cond
                        ; String case
                            [(string-val? current-val)
                                (let* ([str (expval->string current-val)]
                                        [replacement-char (if (string-val? value)
                                                            (if (> (string-length (expval->string value)) 0)
                                                                (string-ref (expval->string value) 0)
                                                                #\space)  ; Default character if empty
                                                            (string-ref (string value) 0))])  ; Convert and take first char
                                (let ([new-str (custom-string-set! str index replacement-char)])
                                    (setref! ref (string-val new-str))))]
                        
                        ; List case
                            ((list-val? current-val)
                                (let* 
                                    (
                                        (lst (expval->list current-val))
                                        (new-list (custom-list-set! lst index value))
                                    )
                                    (setref! ref (list-val new-list))
                                )
                            )
                        
                            (else (error "Variable is not a string or list"))
                        )
                        (error "Undefined variable")
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
                        (else (error "Unknown value type in print" val))
                    )
                )
            )
            (if-exp (condition then-branch else-branch)
                (if (expval->bool (value-of condition))
                    (value-of then-branch)
                    (if else-branch
                        (value-of else-branch)
                        (empty-exp)
                    )
                )
            )
            (block-exp (exprs)
                (value-of-sequence exprs)
            )
            (while-exp (condition body)
                (let loop ()
                    (when (expval->bool (value-of condition))
                        (value-of body)
                        (loop)
                    )
                )
            )
            (for-exp (init condition update body)
                (value-of init)
                (let loop ()
                    (when (expval->bool (value-of condition))
                        (value-of body)
                        (value-of update)
                        (loop)
                    )
                )
            )
            (func-exp (name params body)
                (let 
                    (
                        (param-names 
                            (map 
                                (lambda (p)
                                    (cases expression p
                                        (param-exp (n t) 
                                            n
                                        )
                                        (else (error "Expected a param-exp" p))
                                    )
                                )
                                params
                            )
                        )
                    )
                    (let ((closure (closure-val param-names body (get-env))))
                        (set-env! (extend-env name (newref closure) (get-env)))
                    )
                )
            )
            (call-exp (name args)
                (let ((closure (deref (apply-env name (get-env)))))
                    (cases expval closure
                        (closure-val (param-names body closure-env)
                            (let ((arg-values (map value-of args)))
                                (let ((new-env (foldl (lambda (pair env)
                                    (extend-env (car pair) (newref (cdr pair)) env))
                                    closure-env
                                    (map cons param-names arg-values))))
                                    (let ((old-env (get-env)))
                                        (set-env! new-env)
                                        (let ((result (value-of body)))
                                            (set-env! old-env)
                                            (if (return-signal? result)
                                                (return-signal-value result)
                                                result
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (else
                            (error "Expected a function closure" closure)
                        )
                    )
                )
            )
            (return-exp (exp)
                (return-signal (value-of exp))
            )
            (empty-exp (void))
        )
    )
)

(value-of-program ast)
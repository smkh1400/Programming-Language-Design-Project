#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(define input-port (open-input-file "test.prog"))
(define tokens (lex-all input-port))
;;; (displayln tokens)
(define token-generator (make-token-generator tokens))
(define parse-result (parse-full token-generator))

(close-input-port input-port)

(displayln parse-result)
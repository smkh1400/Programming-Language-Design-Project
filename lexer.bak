#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define full-lexer 
    (lexer
        (whitespace (full-lexer input-port))

        (
            (:or 
                (:: (:+ (char-range #\0 #\9))) ;;;integer like 123 {\d+}
                (:: (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))  ;;;float like 123.456 {\d+}.{\d+}
            )
            (token-NUM (string->number lexeme)) ;;;convert string result to number result
        )

        (
            (:or 
                (:: #\" (:* (:~ #\")) #\")
            )
            (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))
        )

        ((eof) (token-EOF))
        (";" (token-SEMICOLON)) 
        ("break" (token-BREAK)) 
        ("continue" (token-CONTINUE)) 
        ("=" (token-ASSIGNMENT)) 
        ("return" (token-RETURN)) 
        ("func" (token-FUNC)) 
        ("var" (token-VAR))
        ("int" (token-INTTYPE))
        ("float" (token-FLOATTYPE))
        ("string" (token-STRINGTYPE))
        ("bool" (token-BOOL))
        ("list" (token-LIST))
        ("nulltype" (token-NULLTYPE))
        ("print" (token-PRINT))
        ("(" (token-OP)) 
        (")" (token-CP)) 
        (":" (token-COLON)) 
        ("," (token-COMMA))
        ("if" (token-IF)) 
        ("else" (token-ELSE))
        ("for" (token-FOR))
        ("while" (token-WHILE))
        ("||" (token-OR))
        ("&&" (token-AND))
        ("!" (token-NOT))
        ("==" (token-EQUALS))
        ("!=" (token-NOTEQUALS))
        ("<" (token-LT)) 
        ("<=" (token-LET)) 
        (">" (token-GT)) 
        (">=" (token-GET))
        ("+" (token-PLUS)) 
        ("-" (token-MINUS)) 
        ("*" (token-TIMES)) 
        ("/" (token-DIVIDE)) 
        ("%" (token-MODULO)) 
        ("[" (token-OB)) 
        ("]" (token-CB))
        ("{" (token-OCB))
        ("}" (token-CCB))
        ("true" (token-TRUE)) 
        ("false" (token-FALSE)) 
        ("NULL" (token-NULL)) 
        (
            (:+ 
                (:or
                    (char-range #\a #\z)
                    (char-range #\A #\Z)
                    #\_
                )
                (:*
                    (:or 
                        (char-range #\0 #\9) 
                        (char-range #\a #\z) 
                        (char-range #\A #\Z) 
                        #\_
                    )
                )
            ) 
            (token-ID lexeme)
        )
        (any-char (error (format "Unexpected character: ~a" lexeme)))
    )
)

(define-tokens a (NUM ID STRING))
(define-empty-tokens b (EOF SEMICOLON BREAK CONTINUE ASSIGNMENT RETURN FUNC VAR 
                         INTTYPE FLOATTYPE STRINGTYPE BOOL LIST NULLTYPE PRINT
                         OP CP COLON COMMA IF ELSE FOR WHILE OR AND NOT
                         EQUALS NOTEQUALS LT LET GT GET PLUS MINUS TIMES DIVIDE MODULO
                         OB CB OCB CCB TRUE FALSE NULL))

(provide (all-defined-out))

(define (string->input-port str)
  (open-input-string str))

(define (lex-all port)
  (define token (full-lexer port))
  (if (eq? (token-name token) 'EOF)
      (list token)
      (cons token (lex-all port))))

(define (display-tokens tokens)
  (unless (null? tokens)
    (let ([token (car tokens)])
      (printf "~a: ~a\n" 
              (token-name token) 
              (or (token-value token) ""))
    (display-tokens (cdr tokens))))
)

(define test-program 
  "var x: int = 42;
   func foo(a: int): int {
     if (a > 0) {
       return a * 2;
     } else {
       return -1;
     }
   }
   print(foo(x));
   "
)

(define input-port (string->input-port test-program))
(define tokens (lex-all input-port))
(display-tokens tokens)
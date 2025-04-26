#|
MOHAMMADREZA KHOSRAVIAN
SAM KHANAKI
ALI SADEGHI

LEXER FOR TOKENIZING TEXT INPUT CONSIDERING 'GRAMMAR.TXT'
SHARIF UNIVERSITY OF TECHNOLOGY
SPRING 2025
|#

#|
REQUIREMENTS
|#
#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

#|
TOKEN DEFINTIONS
|#
(define-tokens value-tokens
  (INT FLOAT STRING NULL ID TRUE FALSE EOF))

(define-empty-tokens keyword-tokens
  (VAR FUNC IF ELSE WHILE FOR PRINT RETURN BREAK CONTINUE
   INTTYPE FLOATTYPE STRINGTYPE LIST NULLTYPE))

(define-empty-tokens symbol-tokens
  (SEMICOLON COLON COMMA OP CP OB CB OCB CCB
   ASSIGNMENT PLUS MINUS TIMES DIVIDE MODULO
   LT GT LET GET EQUALS NOTEQUALS AND OR NOT))

#|
LINE-NO TRACKING INITIALIZATION
|#
(define line-number 1)

(define (update-line-number lexeme)
  (when (regexp-match? #rx"\n" lexeme) 
    (set! line-number (+ line-number 1))))

#|
LEXER
|#
(define lexer-full 
    (lexer
        ;;Whitespace
        (whitespace (lexer-full input-port))

        ;;Numbers (Int & Float)
        ((:+ (char-range #\0 #\9))
         (token-INT (string->number lexeme)))
        ((:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))
         (token-FLOAT (string->number lexeme)))

        ;;String (anything between double quotes)
        ((:: #\" (:* (:~ #\")) #\")
         (token-STRING (substring lexeme 1 (sub1 (string-length lexeme)))))

        ;;Keywords
        ("var" (token-VAR)) ("func" (token-FUNC)) ("if" (token-IF)) ("else" (token-ELSE))
        ("while" (token-WHILE)) ("for" (token-FOR)) ("print" (token-PRINT))
        ("return" (token-RETURN)) ("break" (token-BREAK)) ("continue" (token-CONTINUE))

        ;;Types
        ("int" (token-INTTYPE)) 
        ("float" (token-FLOATTYPE)) 
        ("string" (token-STRINGTYPE)) 
        ("list" (token-LIST)) 
        ("nulltype" (token-NULLTYPE))

        ;;Operators & Symbols
        ("=" (token-ASSIGNMENT)) ("+" (token-PLUS)) ("-" (token-MINUS))
        ("*" (token-TIMES)) ("/" (token-DIVIDE)) ("%" (token-MODULO))
        ("<" (token-LT)) (">" (token-GT)) ("<=" (token-LET)) (">=" (token-GET))
        ("==" (token-EQUALS)) ("!=" (token-NOTEQUALS)) ("&&" (token-AND)) ("||" (token-OR))
        ("!" (token-NOT))

        ;;Delimiters
        (";" (token-SEMICOLON)) (":" (token-COLON)) ("," (token-COMMA))
        ("(" (token-OP)) (")" (token-CP)) ("[" (token-OB)) ("]" (token-CB))
        ("{" (token-OCB)) ("}" (token-CCB))

        ;;Literals (Booleans & NULL)
        ("true" (token-TRUE)) ("false" (token-FALSE)) ("NULL" (token-NULL))

        ;;Identifiers (Var & Func names)
        ((:+ (:or (char-range #\a #\z) (char-range #\A #\Z) #\_))
         (token-ID lexeme))

        ;;EOF
        ((eof) (token-EOF #f))

        ;;Error handling
        (any-char
         (lambda (lexeme)
           (error (format "Unexpected character '~a' at line ~a"
                          lexeme line-number))))
        )
  )

#|
TOKEN RETRIEVAL FUNCTIONS
    -GET-TOKEN COMMUNICATES WITH THE PARSER, SENDING TOKENS UNTIL
     EOF IS REACHED.
     RETURNS TOKEN | FALSE IF EOF
    -PROVIDE LEXER-FULL AND GET-TOKEN FOR PARSER
|#
(define (get-token input-port)
  (let ([tok (lexer-full input-port)])
    (if (eq? (token-name tok) 'EOF)
        #f
        tok)
    )
  )

(provide lexer-full get-token)

#|
DEBUGGING PURPOSES
|#
(define (string->input-port str)
  (open-input-string str))

(define (lex-all port)
  (define token (lexer-full port))
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
#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)
(require "lexer.rkt")

(define parse-full
  (parser
   (start Program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value)
            (error "Parse error near token:" tok-name)))
   (tokens value-tokens keyword-tokens symbol-tokens)
   (grammar
    [Program
     [(StatementList) (list 'Program $1)]]

    [StatementList
     [(Statement StatementList) (cons $1 $2)]
     [() '()]]

    [Statement
     [(OtherStatement) $1]
     [(IfStatement) $1]]

    [OtherStatement
     [(VariableDeclaration) $1]
     [(FunctionDeclaration) $1]
     [(Assignment) $1]
     [(LoopStatement) $1]
     [(Block) $1]
     [(FunctionCallStmt) $1]
     [(PrintStatement) $1]
     [(ReturnStatement) $1]
     [(BreakStatement) $1]
     [(ContinueStatement) $1]]

    [BreakStatement
     [(BREAK SEMICOLON) (list 'Break)]]

    [ContinueStatement
     [(CONTINUE SEMICOLON) (list 'Continue)]]

    [Block
     [(OCB StatementList CCB) (list 'Block $2)]]

    [VariableDeclaration
     [(VAR ID COLON Type ASSIGNMENT Expression SEMICOLON)
      (list 'VarDecl $2 $4 $6)]]

    [Type
     [(INTTYPE) (list 'Type "int")]
     [(FLOATTYPE) (list 'Type "float")]
     [(STRINGTYPE) (list 'Type "string")]
     [(BOOLTYPE) (list 'Type "bool")]
     [(LIST OB Type CB) (list 'Type "list" $3)]
     [(NULLTYPE) (list 'Type "nulltype")]]

    [FunctionDeclaration
     [(FUNC ID OP ParameterList CP COLON Type Block)
      (list 'FuncDecl $2 $4 $7 $8)]]

    [ParameterList
     [(ParameterListNonEmpty) $1]
     [() '()]]

    [ParameterListNonEmpty
     [(Parameter) (list $1)]
     [(Parameter COMMA ParameterListNonEmpty) (cons $1 $3)]]

    [Parameter
     [(ID COLON Type) (list 'Param $1 $3)]]

    [Assignment
     [(ID ASSIGNMENT Expression SEMICOLON)
      (list 'Assign $1 $3)]]

    [IfStatement
     [(MatchedIf) $1]
     [(UnmatchedIf) $1]]

    [MatchedIf
     [(IF OP Expression CP Block ELSE Block)
      (list 'If $3 $5 $7)]]

    [UnmatchedIf
     [(IF OP Expression CP Block)
      (list 'If $3 $5 'None)]
     [(IF OP Expression CP MatchedIf ELSE UnmatchedIf)
      (list 'If $3 $5 $7)]]

    [LoopStatement
     [(WhileLoop) $1]
     [(ForLoop) $1]]

    [WhileLoop
     [(WHILE OP Expression CP Block)
      (list 'While $3 $5)]]

    [ForLoop
     [(FOR OP ForAssignment SEMICOLON Expression SEMICOLON ForAssignment CP Block)
      (list 'For $3 $5 $7 $9)]]

    [ForAssignment
     [(ID ASSIGNMENT Expression)
      (list 'Assign $1 $3)]]

    [FunctionCallExpr
     [(ID OP ArgumentList CP)
      (list 'Call $1 $3)]]

    [FunctionCallStmt
     [(FunctionCallExpr SEMICOLON)
      $1]]

    [ArgumentList
     [(ArgumentListNonEmpty) $1]
     [() '()]]

    [ArgumentListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ArgumentListNonEmpty)
      (cons $1 $3)]]

    [PrintStatement
     [(PRINT OP Expression CP SEMICOLON)
      (list 'Print $3)]]

    [ReturnStatement
     [(RETURN Expression SEMICOLON)
      (list 'Return $2)]]

    [Expression
     [(SimpleExpression) $1]
     [(SimpleExpression ComparativeOperator Expression)
      (list $2 $1 $3)]]

    [SimpleExpression
     [(Term) $1]
     [(SimpleExpression LogicalOperator Term)
      (list $2 $1 $3)]]

    [Term
     [(Factor) $1]
     [(Term ArithmeticOperator Factor)
      (list $2 $1 $3)]]

    [Factor
     [(ID) $1]
     [(Literal) $1]
     [(OP Expression CP) $2]
     [(FunctionCallExpr) $1]
     [(UnaryOperator Factor)
      (list $1 $2)]
     [(NULL) 'NULL]]

    [Literal
     [(INT) $1]
     [(FLOAT) $1]
     [(STRING) $1]
     [(ListLiteral) $1]
     [(TRUE) 'true]
     [(FALSE) 'false]]

    [ListLiteral
     [(OB ExpressionList CB)
      (list 'List $2)]]

    [ExpressionList
     [(ExpressionListNonEmpty) $1]
     [() '()]]

    [ExpressionListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ExpressionListNonEmpty)
      (cons $1 $3)]]

    [ComparativeOperator
     [(LT) "<"]
     [(GT) ">"]
     [(LET) "<="]
     [(GET) ">="]
     [(EQUALS) "=="]
     [(NOTEQUALS) "!="]]

    [LogicalOperator
     [(AND) "&&"]
     [(OR) "||"]]

    [ArithmeticOperator
     [(PLUS) "+"]
     [(MINUS) "-"]
     [(TIMES) "*"]
     [(DIVIDE) "/"]
     [(MODULO) "%"]]

    [UnaryOperator
     [(MINUS) "-"]
     [(NOT) "!"]]
    )))

(provide parse-full)


;; Debug/Testing code:
;; Create an input-port from a string.
(define (string->input-port str)
  (open-input-string str))

;; Lex all tokens from an input port.
(define (lex-all port)
  (define token (lexer-full port))
  (if (eq? (token-name token) 'EOF)
      (list token)
      (cons token (lex-all port))))

;; Display tokens for debugging.
(define (display-tokens tokens)
  (unless (null? tokens)
    (let ([token (car tokens)])
      (printf "~a: ~a\n" (token-name token) (or (token-value token) ""))
      (display-tokens (cdr tokens)))))

;; Build a token generator from a token list.
(define (make-token-generator tokens)
  (let ([tokens-box (box tokens)])
    (lambda ()
      (let ([lst (unbox tokens-box)])
        (if (null? lst)
            (error "No more tokens")
            (begin
              (set-box! tokens-box (cdr lst))
              (car lst)))))))

;; Sample program to parse:
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
   ")

;; Lex the sample program.
(define input-port (string->input-port test-program))
(define tokens (lex-all input-port))

;; Optionally display tokens for debugging.
(display-tokens tokens)

;; Create a token generator and parse the tokens.
(define token-generator (make-token-generator tokens))
(define ast (parse-full token-generator))

(printf "\nParsed AST:\n~a\n" ast)
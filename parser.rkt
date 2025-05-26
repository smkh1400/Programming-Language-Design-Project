#| 
MOHAMMADREZA KHOSRAVIAN
SAM KHANAKI
ALI SADEGHI

PARSER FOR LANGUAGE IMPLEMENTATION CONSIDERING 'GRAMMAR.TXT'
SHARIF UNIVERSITY OF TECHNOLOGY
SPRING 2025
|#

#|
REQUIREMENTS
|#
#lang racket
(require parser-tools/yacc)
(require parser-tools/lex)
(require "lexer.rkt")

#|
PARSER
|#
(define parse-full
  (parser
   (start Program)
   (end EOF)
   (error 
    (lambda (tok-ok? tok-name tok-value)
      (error "Parse error near token:" tok-name)))
   (tokens value-tokens keyword-tokens symbol-tokens)
   (grammar
    ;------------------------------------------------------------1
    ;  Program → StatementList
    ;------------------------------------------------------------
    [Program
     [(StatementList) (list 'Program $1)]]

    ;------------------------------------------------------------2
    ;  StatementList → Statement StatementList | ε
    ;------------------------------------------------------------
    [StatementList
     [(Statement StatementList) (cons $1 $2)]
     [() '()]]

    ;------------------------------------------------------------3
    ;  Statement → VariableDeclaration
    ;              | FunctionDeclaration
    ;              | Assignment
    ;              | IfStatement
    ;              | LoopStatement
    ;              | Block
    ;              | FunctionCall
    ;              | PrintStatement
    ;              | ReturnStatement
    ;              | BreakStatement
    ;              | ContinueStatement
    ;------------------------------------------------------------
    [Statement
     [(VariableDeclaration) $1]
     [(FunctionDeclaration) $1]
     [(Assignment) $1]
     [(IfStatement) $1]
     [(LoopStatement) $1]
     [(Block) $1]
     [(FunctionCallStmt) $1]
     [(PrintStatement) $1]
     [(ReturnStatement) $1]
     [(BreakStatement) $1]
     [(ContinueStatement) $1]]

    ;------------------------------------------------------------4
    ;  BreakStatement → 'break' ';'
    ;------------------------------------------------------------
    [BreakStatement
     [(BREAK SEMICOLON) (list 'Break)]]

    ;------------------------------------------------------------5
    ;  ContinueStatement → 'continue' ';'
    ;------------------------------------------------------------
    [ContinueStatement
     [(CONTINUE SEMICOLON) (list 'Continue)]]

    ;------------------------------------------------------------6
    ;  Block → '{' StatementList '}'
    ;------------------------------------------------------------
    [Block
     [(OCB StatementList CCB) (list 'Block $2)]]

    ;------------------------------------------------------------7
    ;  VariableDeclaration → 'var' Identifier ':' Type '=' Expression ';'
    ;------------------------------------------------------------
    [VariableDeclaration
     [(VAR ID COLON Type ASSIGNMENT Expression SEMICOLON)
      (list 'VarDecl $2 $4 $6)]]

    ;------------------------------------------------------------8
    ;  Type → 'int'
    ;          | 'float'
    ;          | 'string'
    ;          | 'bool'
    ;          | 'list' '[' Type ']'
    ;          | 'nulltype'
    ;------------------------------------------------------------
    [Type
     [(INTTYPE) (list 'Type "int")]
     [(FLOATTYPE) (list 'Type "float")]
     [(STRINGTYPE) (list 'Type "string")]
     [(BOOLTYPE) (list 'Type "bool")]
     [(LIST OB Type CB) (list 'Type "list" $3)]
     [(NULLTYPE) (list 'Type "nulltype")]]

    ;------------------------------------------------------------9
    ;  FunctionDeclaration → 'func' Identifier '(' ParameterList ')' ':' Type Block
    ;------------------------------------------------------------
    [FunctionDeclaration
     [(FUNC ID OP ParameterList CP COLON Type Block)
      (list 'FuncDecl $2 $4 $7 $8)]]

    ;------------------------------------------------------------10
    ;  ParameterList → Parameter ',' ParameterList | Parameter | ε
    ;------------------------------------------------------------
    [ParameterList
     [(ParameterListNonEmpty) $1]
     [() '()]]
    [ParameterListNonEmpty
     [(Parameter) (list $1)]
     [(Parameter COMMA ParameterListNonEmpty) (cons $1 $3)]]

    ;------------------------------------------------------------11
    ;  Parameter → Identifier ':' Type
    ;------------------------------------------------------------
    [Parameter
     [(ID COLON Type) (list 'Param $1 $3)]]

    ;------------------------------------------------------------12
    ;  Assignment → Identifier '=' Expression ';'
    ;------------------------------------------------------------
    [Assignment
     [(ID ASSIGNMENT Expression SEMICOLON)
      (list 'Assign $1 $3)]
     [(ID OB Expression CB ASSIGNMENT Expression SEMICOLON)
      (list 'AssignIndex $1 $3 $6)]]

    ;------------------------------------------------------------13
    ;  IfStatement → MatchedIf | UnmatchedIf
    ;------------------------------------------------------------
    [IfStatement
     [(MatchedIf) $1]
     [(UnmatchedIf) $1]]

    ;------------------------------------------------------------14
    ;  MatchedIf → 'if' '(' Expression ')' Block 'else' Block
    ;------------------------------------------------------------
    [MatchedIf
     [(IF OP Expression CP Block ELSE Block)
      (list 'If $3 $5 $7)]]

    ;------------------------------------------------------------15
    ;  UnmatchedIf → 'if' '(' Expression ')' Block
    ;               | 'if' '(' Expression ')' MatchedIf 'else' UnmatchedIf
    ;------------------------------------------------------------
    [UnmatchedIf
     [(IF OP Expression CP Block)
      (list 'If $3 $5 'None)]
     [(IF OP Expression CP MatchedIf ELSE UnmatchedIf)
      (list 'If $3 $5 $7)]]

    ;------------------------------------------------------------16
    ;  LoopStatement → 'while' '(' Expression ')' Block
    ;                    | 'for' '(' ForAssignment ';' Expression ';' ForAssignment ')' Block
    ;------------------------------------------------------------
    [LoopStatement
     [(WhileLoop) $1]
     [(ForLoop) $1]]

    ;------------------------------------------------------------17
    ;  WhileLoop → 'while' '(' Expression ')' Block
    ;------------------------------------------------------------
    [WhileLoop
     [(WHILE OP Expression CP Block)
      (list 'While $3 $5)]]

    ;------------------------------------------------------------18
    ;  ForLoop → 'for' '(' ForAssignment ';' Expression ';' ForAssignment ')' Block
    ;------------------------------------------------------------
    [ForLoop
     [(FOR OP ForAssignment SEMICOLON Expression SEMICOLON ForAssignment CP Block)
      (list 'For $3 $5 $7 $9)]]

    ;------------------------------------------------------------19
    ;  ForAssignment → Identifier '=' Expression
    ;------------------------------------------------------------
    [ForAssignment
     [(ID ASSIGNMENT Expression)
      (list 'Assign $1 $3)]]

    ;------------------------------------------------------------20
    ;  ArgumentList → Expression ',' ArgumentList
    ;                  | Expression
    ;                  | ε
    ;------------------------------------------------------------
    [ArgumentList
     [(ArgumentListNonEmpty) $1]
     [() '()]]
    [ArgumentListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ArgumentListNonEmpty)
      (cons $1 $3)]]

    ;------------------------------------------------------------21
    ;  FunctionCall → Identifier '(' ArgumentList ')' 
    ;  (as an expression: FunctionCallExpr, and with ';' for statements: FunctionCallStmt)
    ;------------------------------------------------------------
    [FunctionCallExpr
     [(ID OP ArgumentList CP)
      (list 'Call $1 $3)]]
    [FunctionCallStmt
     [(FunctionCallExpr SEMICOLON)
      $1]]

    ;------------------------------------------------------------22
    ;  PrintStatement → 'print' '(' Expression ')' ';'
    ;------------------------------------------------------------
    [PrintStatement
     [(PRINT OP Expression CP SEMICOLON)
      (list 'Print $3)]]

    ;------------------------------------------------------------23
    ;  ReturnStatement → 'return' Expression ';'
    ;------------------------------------------------------------
    [ReturnStatement
     [(RETURN Expression SEMICOLON)
      (list 'Return $2)]]

    ;------------------------------------------------------------24
    ;  Expression → SimpleExpression
    ;                | SimpleExpression ComparativeOperator Expression
    ;------------------------------------------------------------
    [Expression
     [(SimpleExpression) $1]
     [(SimpleExpression ComparativeOperator Expression)
      (list $2 $1 $3)]]

    ;------------------------------------------------------------25
    ;  SimpleExpression → Term
    ;                      | SimpleExpression LogicalOperator Term
    ;------------------------------------------------------------
    [SimpleExpression
     [(Term) $1]
     [(SimpleExpression LogicalOperator Term)
      (list $2 $1 $3)]]

    ;------------------------------------------------------------26
    ;  Term → Factor
    ;         | Term ArithmeticOperator Factor
    ;------------------------------------------------------------
    [Term
     [(Factor) $1]
     [(Term ArithmeticOperator Factor)
      (list $2 $1 $3)]]

    ;------------------------------------------------------------27
    ;  Factor → Identifier
    ;     | Identifier '[' Expression ']'
    ;     | Literal
    ;     | '(' Expression ')'
    ;     | FunctionCall
    ;     | UnaryOperator Factor
    ;     | 'NULL'
    ;------------------------------------------------------------
    [Indexing
     [(OB Expression CB) (list $2)]]

    [OptionalIndexing
     [() '()] ; empty
     [(Indexing) $1]] ; presence of indexing

    [Factor
     [(ID OptionalIndexing) 
      (if (null? $2)
          $1
          (list 'Index $1 (car $2)))]
     [(Literal) $1]
     [(OP Expression CP) $2]
     [(FunctionCallExpr) $1]
     [(UnaryOperator Factor) (list $1 $2)]
     [(NULL) $1]]


    ;------------------------------------------------------------28
    ;  Literal → IntegerLiteral | FloatLiteral | StringLiteral
    ;              | ListLiteral | 'true' | 'false'
    ;------------------------------------------------------------
    [Literal
     [(INT) $1]
     [(FLOAT) $1]
     [(STRING) $1]
     [(ListLiteral) $1]
     [(TRUE) $1]
     [(FALSE) $1]]

    ;------------------------------------------------------------29
    ;  ListLiteral → '[' ExpressionList ']'
    ;------------------------------------------------------------
    [ListLiteral
     [(OB ExpressionList CB)
      (list 'List $2)]]

    ;------------------------------------------------------------30
    ;  ExpressionList → Expression ',' ExpressionList
    ;                  | Expression | ε
    ;------------------------------------------------------------
    [ExpressionList
     [(ExpressionListNonEmpty) $1]
     [() '()]]
    [ExpressionListNonEmpty
     [(Expression) (list $1)]
     [(Expression COMMA ExpressionListNonEmpty)
      (cons $1 $3)]]

    ;------------------------------------------------------------31
    ;  ComparativeOperator → '<' | '>' | '<=' | '>=' | '==' | '!='
    ;------------------------------------------------------------
    [ComparativeOperator
     [(LT) "<"]
     [(GT) ">"]
     [(LET) "<="]
     [(GET) ">="]
     [(EQUALS) "=="]
     [(NOTEQUALS) "!="]]

    ;------------------------------------------------------------32
    ;  LogicalOperator → '&&' | '||'
    ;------------------------------------------------------------
    [LogicalOperator
     [(AND) "&&"]
     [(OR) "||"]]

    ;------------------------------------------------------------33
    ;  ArithmeticOperator → '+' | '-' | '*' | '/' | '%'
    ;------------------------------------------------------------
    [ArithmeticOperator
     [(PLUS) "+"]
     [(MINUS) "-"]
     [(TIMES) "*"]
     [(DIVIDE) "/"]
     [(MODULO) "%"]]

    ;------------------------------------------------------------34
    ;  UnaryOperator → '-' | '!'
    ;------------------------------------------------------------
    [UnaryOperator
     [(MINUS) "-"]
     [(NOT) "!"]]
   )))

(provide parse-full)

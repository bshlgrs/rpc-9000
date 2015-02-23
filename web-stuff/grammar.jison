/* description: Parses end executes mathematical expressions. */

/* lexical grammar */
%lex
%%

\s+                   /* skip whitespace */
[0-9]+?\b             return 'NUMBER'
"*"                   return '*'
"/"                   return '/'
"-"                   return '-'
"+"                   return '+'
"%"                   return '%'
"("                   return '('
")"                   return ')'
"=="                  return '=='
">"                   return '>'
">="                   return '>='
"="                   return '='
";"                   return ';'
"int"                 return 'INT'
"if"                  return 'IF'
"else"                return 'ELSE'
"void"                return 'VOID'
"return"              return 'RETURN'
"{"                   return '{'
"}"                   return '}'
","                   return ','
<<EOF>>               return 'EOF'
[a-zA-Z_]+            return 'NAME'
.                     return 'INVALID'

/lex

/* operator associations and precedence */

%left '+' '-'
%left '*' '/' '%'
%left UMINUS

%start program

%% /* language grammar */

program
    : listOfFunctions
        { return $1;}
    ;

listOfFunctions
    : function listOfFunctions
        {$$ = [$1].concat($2);}
    | function EOF
        {$$ = [$1];}
    ;

function
    : INT NAME '(' listOfArgs '{' body '}'
        {$$ = {type: "FunctionDef", name: $2, args: $4, body: $6};}
    ;

listOfArgs
    : arg ')'
        {$$ = [$1];}
    | arg ',' listOfArgs
        {$$ = [$1].concat($3);}
    ;

listOfFunctionArgs
    : expr ')'
        {$$ = [$1];}
    | ')'
        {$$ = [];}
    | expr ',' listOfFunctionArgs
        {$$ = [$1].concat($3);}
    ;

body
    : stat body
        {$$ = [$1].concat($2);}
    | stat
        {$$ = [$1];}
    ;

arg
    : type NAME
        {$$ = {name: $2, ctype: $1};}
    ;

type
    : INT
        {$$ = ["Int"];}
    ;

stat
    : NAME '=' expr ';'
        {$$ = {type: "Assignment", name: $1, rhs: $3};}
    | RETURN ';'
        {$$ = {type: "Return"};}
    | RETURN expr ';'
        {$$ = {type: "Return", value: $2};}
    | IF '(' boolExpr ')' '{' body '}'
        {$$ = {type: "IfElse", condition: $3, thenBlock: $6};}
    | IF '(' boolExpr ')' '{' body '}' ELSE '{' body '}'
        {$$ = {type: "IfElse", condition: $3, thenBlock: $6, elseBlock: $10};}
    ;

expr
    : expr '+' expr
        {$$ = {type: "BinOp", op: "+", lhs: $1, rhs: $3};}
    | expr '-' expr
        {$$ = {type: "BinOp", op: "-", lhs: $1, rhs: $3};}
    | expr '*' expr
        {$$ = {type: "BinOp", op: "*", lhs: $1, rhs: $3};;}
    | expr '/' expr
        {$$ = {type: "BinOp", op: "/", lhs: $1, rhs: $3};}
    | expr '%' expr
        {$$ = {type: "BinOp", op: "%", lhs: $1, rhs: $3};}
    | '-' expr %prec UMINUS
        {$$ = {type: "BinOp", op: "-", lhs: {type: "Lit", value: 0}, rhs: $3};}
    | '(' expr ')'
        {$$ = $2;}
    | NAME '(' listOfFunctionArgs
        {$$ = {type: "FunctionCall", name: $1, args: $3};}
    | NAME
        {$$ = {type: "Var", name: $1};}
    | NUMBER
        {$$ = {type: "Lit", value: $1};}
    ;

boolBinOperator
    : '=='
        {$$ = $1}
    | '>'
        {$$ = $1}
    | '>='
        {$$ = $1}
    ;

boolExpr
    : expr boolBinOperator expr
        {$$ = {type: "BooleanBinOp", op: $2, lhs: $1, rhs: $3};}
    ;

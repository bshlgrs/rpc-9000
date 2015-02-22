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
"^"                   return '^'
"("                   return '('
")"                   return ')'
"="                   return '='
";"                   return ';'
"int"                 return 'INT'
"void"                return 'VOID'
"return"                return 'RETURN'
"{"                   return '{'
"}"                   return '}'
","                   return ','
<<EOF>>               return 'EOF'
[a-zA-Z_]+            return 'NAME'
.                     return 'INVALID'

/lex

/* operator associations and precedence */

%left '+' '-'
%left '*' '/'
%left '^'
%left UMINUS

%start program

%% /* language grammar */

program
    : function EOF
        {return $1;}
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
    | '-' expr %prec UMINUS
        {$$ = {type: "BinOp", op: "-", lhs: {type: "Lit", value: 0}, rhs: $3};}
    | '(' expr ')'
        {$$ = $2;}
    | NAME
        {$$ = {type: "Var", name: $1};}
    | NUMBER
        {$$ = {type: "Lit", value: $1};}
    ;

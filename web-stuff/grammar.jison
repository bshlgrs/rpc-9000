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
        {$$ = ["FunctionDef", $2, $4, $6];}
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
        {$$ = [$1, $2];}
    ;

type
    : INT
        {$$ = ["Int"];}
    ;

stat
    : NAME '=' expr ';'
        {$$ = {type: "Assignment", name: $1, rhs: $3};}
    | RETURN ';'
        {$$ = ["Return"];}
    | RETURN expr ';'
        {$$ = ["Return", $2];}
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
        {$$ = {type: "BinOp", op: "-", lhs: ["IntLit", 0], rhs: $3};}
    | '(' expr ')'
        {$$ = $2;}
    | NAME
        {$$ = {type: "Var", name: $1];}
    | NUMBER
        {$$ = {type: "Lit", value: Number(yytext)];}
    ;

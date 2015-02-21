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
    : stat EOF
        {return $1;}
    ;

stat
    : NAME '=' expr ';'
        {return ["Assignment", $1, $3];}
    ;

expr
    : expr '+' expr
        {$$ = ["BinOp", "+", $1, $3];}
    | expr '-' expr
        {$$ = ["BinOp", "-", $1, $3];}
    | expr '*' expr
        {$$ = ["BinOp", "*", $1, $3];;}
    | expr '/' expr
        {$$ = ["BinOp", "/", $1, $3];}
    | '-' expr %prec UMINUS
        {$$ = ["BinOp", "-", ["IntLit", 0], $3];}
    | '(' expr ')'
        {$$ = $2;}
    | NAME
        {$$ = ["Var", $1];}
    | NUMBER
        {$$ = ["IntLit", Number(yytext)];}
    ;

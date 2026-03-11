/* Simple calculator parser for use with sedlex via ocamlyacc */

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start main
%type <int> main

%%

main:
  | expr EOF { $1 }
;

expr:
  | INT { $1 }
  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr TIMES expr { $1 * $3 }
  | expr DIV expr { $1 / $3 }
;

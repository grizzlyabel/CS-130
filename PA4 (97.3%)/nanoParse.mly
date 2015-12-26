%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF
%token TRUE
%token FALSE
%token <string> Id
%token LET REC EQ IN FUN ARROW IF THEN ELSE
%token PLUS MINUS MUL DIV LE LT NE AND OR 
%token LPAREN RPAREN
%token LBRAC RBRAC COLONCOLON SEMI APP

%nonassoc LET IF
%left OR
%left AND
%left EQ NE LT LE
%right COLONCOLON SEMI RBRAC
%left PLUS MINUS
%left MUL DIV
%left APP FUN

%start exp 
%type <Nano.expr> exp

%%

exp:
  | LET Id EQ exp IN exp      { Let($2,$4,$6) }
  | LET REC Id EQ exp IN exp  { Letrec($3,$5,$7) }
  | FUN Id ARROW exp          { Fun($2,$4) }
  | IF exp THEN exp ELSE exp  { If($2,$4,$6) }
  | exp2                      { $1 }

exp2:
  | exp2 OR exp3    { Bin($1,Or,$3) }
  | exp3            { $1 }

exp3:
  | exp3 AND exp4   { Bin($1,And,$3) }
  | exp4            { $1 }

exp4:
  | exp4 EQ exp5    { Bin($1, Eq, $3) }
  | exp4 NE exp5    { Bin($1, Ne, $3) }
  | exp4 LT exp5    { Bin($1, Lt, $3) }
  | exp4 LE exp5    { Bin($1, Le, $3) }
  | exp5            { $1 }

exp5:
  | exp6 COLONCOLON exp5  { Bin($1, Cons, $3) }
  | exp6 SEMI exp5        { Bin($1, Cons, $3) }
  | exp5 RBRAC            { Bin($1, Cons, NilExpr) }
  | LBRAC exp5            { $2 }
  | exp6                  { $1 }

exp6:
  | exp6 PLUS exp7  { Bin($1, Plus, $3) }
  | exp6 MINUS exp7 { Bin($1, Minus, $3) }
  | exp7            { $1 }

exp7:
  | exp7 MUL exp8   { Bin($1, Mul, $3) }
  | exp7 DIV exp8   { Bin($1, Div, $3) }
  | exp8            { $1 }

exp8:
  | exp8 exp9       { App($1, $2) }
  | exp9            { $1 }

exp9:
  | Num             { Const $1 }
  | TRUE            { True }
  | FALSE           { False }
  | Id              { Var $1 }
  | LPAREN exp RPAREN { $2 }
  | LBRAC RBRAC       { NilExpr }


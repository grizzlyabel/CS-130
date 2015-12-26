{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}
rule token = parse
    eof         { EOF }
  | ['\t' ' ' '\n' '\r'] { token lexbuf }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  
  | "["         { LBRAC }
  | "]"         { RBRAC }
  
  | "*"         { MUL }
  | "/"         { DIV }
  
  | "+"         { PLUS }
  | "-"         { MINUS }
  
  | "::"        { COLONCOLON }
  | ";"         { SEMI }
  
  | "="         { EQ }
  | "<="        { LE }
  | "<"         { LT }
  | "!="        { NE }
  
  | "&&"        { AND }
  | "||"        { OR }
  
  | "false"     { FALSE }
  | "true"      { TRUE }
  | ['0'-'9']+ as a { Num(int_of_string a) }
  | "let"       { LET }
  | "rec"       { REC }
  | "in"        { IN }
  | "fun"       { FUN }
  | "->"        { ARROW }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as x { Id (x) }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }

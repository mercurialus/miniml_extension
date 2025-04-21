{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+
let digit = ['0'-'9']
let string_char = [^ '"' '\\']  (* any char except "" and \ *)
let escape = '\\' ['\\' '"' 'n' 't' 'r']

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"           { TINT }
  | "bool"          { TBOOL }
  | "string"        { TSTRING }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "is"            { IS }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }  
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIV }
  | "try"           { TRY }
  | "with"          { WITH }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | '"' (string_char | escape)* '"'    {  (* ← use your let-bound patterns *)
     let raw = Lexing.lexeme lexbuf in
     let len = String.length raw in
     let content = String.sub raw 1 (len - 2) in
     STRING content
   }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}

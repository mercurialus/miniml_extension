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
  | "raise"         { RAISE }
  | "DivisionByZero" { DIVZERO }
  | "GenericException" { GENERIC }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | '"' (string_char | escape)* '"'    {  
    let raw = Lexing.lexeme lexbuf in
    let len = String.length raw in
    let inner = String.sub raw 1 (len - 2) in
    let unescape s =
    let buf = Buffer.create (String.length s) in
    let rec loop i =
    if i = String.length s then Buffer.contents buf else
    match s.[i] with
    | '\\' when i + 1 < String.length s ->
        begin match s.[i+1] with
        | 'n'  -> Buffer.add_char buf '\n'; loop (i+2)
        | 't'  -> Buffer.add_char buf '\t'; loop (i+2)
        | 'r'  -> Buffer.add_char buf '\r'; loop (i+2)
        | '\\' -> Buffer.add_char buf '\\'; loop (i+2)
        | '"'  -> Buffer.add_char buf '"';  loop (i+2)
        | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c; loop (i+2)
        end
    | c -> Buffer.add_char buf c; loop (i+1)
  in loop 0 in 
  let content = unescape inner in
     STRING content
   }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}

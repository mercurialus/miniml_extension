%{
  open Syntax
%}

%token TINT
%token TBOOL
%token TARROW
%token TSTRING

%token <Syntax.name> VAR
%token <int> INT
%token <string> STRING (*Creating a new token for string*)
%token TRUE FALSE
%token PLUS
%token MINUS
%token DIV (*New token for division*)
%token TIMES
%token EQUAL LESS
%token IF THEN ELSE
%token FUN IS
%token COLON
%token LPAREN RPAREN
%token LET
%token SEMISEMI
%token EOF
%token TRY WITH LBRACE RBRACE (*New tokens for try-with*)
%token RAISE                  (*New token for raise*)
%token DIVZERO                (*DivisionByZero literal*)
%token GENERIC                (*GenericException literal*)

%start file
%type <Syntax.command list> file

%start toplevel
%type <Syntax.command> toplevel

%nonassoc IS
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIV
%right TARROW

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | ds = nonempty_list(def) SEMISEMI lst = file
    { ds @ lst }
  | ds = nonempty_list(def) EOF
    { ds }

toplevel:
  | d = def SEMISEMI
    { d }
  | e = expr SEMISEMI
    { Expr e }

def:
  | LET x = VAR EQUAL e = expr
    { Def (x, e) }

expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | MINUS n = INT
    { Int (-n) }
  | e1 = expr PLUS e2 = expr	
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr DIV e2 = expr
    { Div (e1,e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr EQUAL e2 = expr
    { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr
    { Less (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If (e1, e2, e3) }
  | FUN x = VAR LPAREN f = VAR COLON t1 = ty RPAREN COLON t2 = ty IS e = expr
    { Fun (x, f, t1, t2, e) }
  | RAISE DIVZERO
    { Raise DivZero }
  | RAISE GENERIC
    { Raise Generic }
  /* ——— Exactly two‐case try‐with ——— */
  | TRY LBRACE e1 = expr RBRACE WITH LBRACE DIVZERO TARROW e2 = expr RBRACE
    { TryWith (e1, DivZero, e2) }
  | TRY LBRACE e1 = expr RBRACE WITH LBRACE GENERIC TARROW e2 = expr RBRACE
    { TryWith (e1, Generic,  e2) }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | e1 = app_expr e2 = simple_expr
    { Apply (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | x = VAR
    { Var x }
  | s = STRING
    {String s}  (*New Variable data type*)
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | n = INT
    { Int n }
  | LPAREN e = plain_expr RPAREN	
    { e }    

ty:
  | TBOOL
    { TBool }
  | TINT
    { TInt }
  | TSTRING
    { TString } (*String token*)
  | t1 = ty TARROW t2 = ty
    { TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }

mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }

%%


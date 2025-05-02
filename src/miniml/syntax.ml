(* Abstract syntax. *)

(* Variable names *)
type name = string

(* Types *)
type ty =
  | TInt              (* Integers *)
  | TBool             (* Booleans *)
  | TString           (* String, new type*)
  | TArrow of ty * ty (* Functions *)

type exn_type=
  | DivZero
  | Generic

(* Expressions *)
type expr = expr' Zoo.located
and expr' =
  | Var of name          		(* Variable *)
  | Int of int           		(* Non-negative integer constant *)
  | Bool of bool         		(* Boolean constant *)
  | String of string        (*String literal*)
  | Times of expr * expr 		(* Product [e1 * e2] *)
  | Div of expr * expr      (* Divsion [e1 / e2] *)
  | Plus of expr * expr  		(* Sum [e1 + e2] *)
  | Minus of expr * expr 		(* Difference [e1 - e2] *)
  | Equal of expr * expr 		(* Integer comparison [e1 = e2] *)
  | Less of expr * expr  		(* Integer comparison [e1 < e2] *)
  | If of expr * expr * expr 		(* Conditional [if e1 then e2 else e3] *)
  | Fun of name * name * ty * ty * expr (* Function [fun f(x:s):t is e] *)
  | Apply of expr * expr 		(* Application [e1 e2] *)
  | TryWith of expr * exn_type * expr  (* Exception handling [try e1 with x is e2] *)
  | Raise of exn_type (* Raise an exception [raise e] *)



(* Toplevel commands *)
type command =
  | Expr of expr       (* Expression *)
  | Def of name * expr (* Value definition [let x = e] *)

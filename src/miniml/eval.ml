(** Evaluation rules, small-step operational semantics.

   This module is for demonstration purposes only. It is inefficient
   and not used by the toplevel, which compiles programs to "machine"
   language, see modules Machine and Compile.
*)

open Syntax

(** [is_value e] returns true, if program [e] is a value. *)
let is_value = function
  | Int _ | Bool _ | String _| Fun _ -> true
  | Var _ | Times _ | Plus _ | Minus _
  | Equal _ | Less _ | If _ | Apply _ -> false

(** An exception indicating a value. *)
exception Value

(** An exception indicating a runtime error. *)
exception Runtime

(**Generic and DivZero**)
exception EDivZero
exception EGeneric

(** [eval1 e] performs a single evaluation step. It raises exception
    Value if [e] is a value. *)
let rec eval1 = function
  | Var _ -> raise Runtime
  | Int _ | Bool _ | Fun _ -> raise Value
  | Raise DivZero -> raise EDivZero
  | Raise Generic -> raise EGeneric
  | Times (Int k1, Int k2) -> Int (k1 * k2)
  | Times (Int k1, e2)     -> Times (Int k1, eval1 e2)
  | Times (e1, e2)         -> Times (eval1 e1, e2)
  | Plus (Int k1, Int k2)  -> Int (k1 + k2)
  | Plus (Int k1, e2)      -> Plus (Int k1, eval1 e2)
  | Plus (e1, e2)          -> Plus (eval1 e1, e2)
  | Minus (Int k1, Int k2) -> Int (k1 - k2)
  | Minus (Int k1, e2)     -> Minus (Int k1, eval1 e2)
  | Minus (e1, e2)         -> Minus (eval1 e1, e2)
  | Equal (Int k1, Int k2) -> Bool (k1 = k2)
  | Equal (Int k1, e2)     -> Equal (Int k1, eval1 e2)
  | Equal (e1, e2)         -> Equal (eval1 e1, e2)
  | Less (Int k1, Int k2)  -> Bool (k1 < k2)
  | Less (Int k1, e2)      -> Less (Int k1, eval1 e2)
  | Less (e1, e2)          -> Less (eval1 e1, e2)
  | If (Bool true, e2, e3) -> e2
  | If (Bool false, e2, e3)-> e3
  | If (e1, e2, e3)        -> If (eval1 e1, e2, e3)
  | Apply (Fun (f, x, _, _, e) as v1, v2) when is_value v2 ->
      subst [(f, v1); (x, v2)] e
  | Apply (Fun _ as v1, e2) -> Apply (v1, eval1 e2)
  | Apply (e1, e2) -> Apply (eval1 e1, e2)
  | Div (v1, v2) when is_int v1 && is_int v2 && int_of v2 = 0 ->
    raise EDivZero                        (* automatic DBZ detection *)

    | TryWith (body, kind, handler) ->
           begin
             try
               eval1 body      (* big-step the entire body *)
             with
             | EDivZero when kind = DivZero -> handler
             | EGeneric when kind = Generic  -> handler
             | exn -> raise exn             (* propagate anything else *)
           end

(** [eval e] evaluates program [e]. The evaluation returns a value,
    diverges, or raises the [Runtime] exception. *)
let rec eval e =
  let rec loop e = if is_value e then e else loop (eval1 e)
  in
    loop e

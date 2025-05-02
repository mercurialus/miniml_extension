(** MiniML compiler. *)

open Machine

(** [compile e] compiles program [e] into a list of machine instructions. *)
let rec compile {Zoo.data=e'; _} =
  match e' with
    | Syntax.Var x -> [IVar x]
    | Syntax.Int k -> [IInt k]
    | Syntax.String s -> [IString s]
    | Syntax.Bool b -> [IBool b]
    | Syntax.Times (e1, e2) -> (compile e1) @ (compile e2) @ [IMult]
    | Syntax.Div   (e1, e2) -> (compile e1) @ (compile e2) @ [IDiv]
    | Syntax.Plus  (e1, e2) -> (compile e1) @ (compile e2) @ [IAdd]
    | Syntax.Minus (e1, e2) -> (compile e1) @ (compile e2) @ [ISub]
    | Syntax.Equal (e1, e2) -> (compile e1) @ (compile e2) @ [IEqual]
    | Syntax.Less  (e1, e2) -> (compile e1) @ (compile e2) @ [ILess]
    | Syntax.If (e1, e2, e3) ->
        (compile e1) @ [IBranch (compile e2, compile e3)]
    | Syntax.Fun (f, x, _, _, e) ->
        [IClosure (f, x, compile e @ [IPopEnv])]
    | Syntax.Apply (e1, e2) ->
        (compile e1) @ (compile e2) @ [ICall]

    (* --- new two‑case try‑with ------------------------------------ *)
    | Syntax.TryWith (e1, Syntax.DivZero, handler) ->
        [IHandleDivZero (compile handler)] @ (compile e1)
    | Syntax.TryWith (e1, Syntax.Generic , handler) ->
        [IHandleGeneric (compile handler)] @ (compile e1)

    (* --- raising one of the two sentinel exceptions --------------- *)
    | Syntax.Raise Syntax.DivZero ->
        [IInt 0; IRaise]          (* DivisionByZero sentinel = 0 *)
    | Syntax.Raise Syntax.Generic ->
        [IInt 1; IRaise]          (* GenericException sentinel  = 1 *)

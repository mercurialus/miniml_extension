(** Type checking. *)

open Syntax

let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc

(** [check ctx ty e] verifies that expression [e] has type [ty] in
    context [ctx]. If it does, it returns unit, otherwise it raises the
    [Type_error] exception. *)
let rec check ctx ty ({Zoo.loc;_} as e) =
  let ty' = type_of ctx e in
    if ty' <> ty then
      typing_error ~loc
        "This expression has type %t but is used as if it has type %t"
        (Print.ty ty')
        (Print.ty ty)

(** [type_of ctx e] computes the type of expression [e] in context
    [ctx]. If [e] does not have a type it raises the [Type_error]
    exception. *)
and type_of ctx {Zoo.data=e; loc} =
  match e with
    | Var x ->
        (try List.assoc x ctx with
         | Not_found -> typing_error ~loc "unknown variable %s" x)

    | Int _  -> TInt
    | Bool _ -> TBool
    | String _ -> TString                      (*NEW *)
    | Times (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    
    | Plus (e1, e2) ->
        begin match (type_of ctx e1, type_of ctx e2) with
        | TInt   , TInt    -> TInt
        | TString, TString -> TString
        | ty1   , ty2 ->
            typing_error ~loc
              "both sides of + must be int or both string (got %t and %t)"
              (Print.ty ty1) (Print.ty ty2)
        end

    | Minus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
    | Div   (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt

    | Equal (e1, e2) ->
        begin match (type_of ctx e1, type_of ctx e2) with
        | TInt   , TInt
        | TString, TString -> TBool
        | ty1   , ty2 ->
            typing_error ~loc
              "cannot compare values of types %t and %t"
              (Print.ty ty1) (Print.ty ty2)
        end

    | Less (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
    | If (e1, e2, e3) ->
      check ctx TBool e1 ;
      let ty = type_of ctx e2 in
	check ctx ty e3 ; ty
    | Raise e ->
      check ctx TInt e ;
      TInt
    (* The type of the exception is the same as the type of the raised
       expression. *)
    | TryWith (e1, e2) ->(
      let ty1 = type_of ctx e1 in
      let ty2 = type_of ctx e2 in
      if ty1 = ty2 then 
        ty1
      else 
        typing_error ~loc
          "this expression is used as an exception but its type is %t" (Print.ty ty1))
    | Fun (f, x, ty1, ty2, e) ->
      check ((f, TArrow(ty1,ty2)) :: (x, ty1) :: ctx) ty2 e ;
      TArrow (ty1, ty2)
    | Apply (e1, e2) ->
      begin match type_of ctx e1 with
	  TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2
	| ty ->
	  typing_error ~loc
            "this expression is used as a function but its type is %t" (Print.ty ty)
      end

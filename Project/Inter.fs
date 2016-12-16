(* File Project/Inter.fs
*)

module Inter

open Absyn
open Env

(* A runtime value is an integer, a list, or a function closure *)

type value = 
  | Int of int
  | List of value list
  | Closure of string option * string * expr * value env   
                                                                (* (Some f, x, e, env) where f is name of the function, 
                                                                x is the name of the input parameter, e is the body 
                                                                of the function, and env is the environment for the free 
                                                                (non-local) variables in e. A value env is an environment that maps a name (a string) to a corresponding
                                                                 value;*)


let rec toString v t = 
  match (v, t) with                            (*Value, type*)
  | (_, AnyT)          -> "value" 
  | (_, ArrowT (_, _)) -> "closure"  
  | (_, UnitT)         -> "null"
  | (Int i, IntT)      -> string i
  | (Int 0, BoolT)     -> "false"
  | (Int 1, BoolT)     -> "true"
  | (List l, ListT t1) -> "[" + listToString l t1 + "]"
  | _ ->  failwith "toString: mismatched type and value"
and listToString l t =
  match l with
  |      [] -> ""
  | v :: [] -> toString v t
  | v :: vs -> (toString v t) + "; " + (listToString vs t)

  (* Language interpreter *)

let rec eval (e : expr) (env : value env) : value =
    match e with
    | (Con n, h) -> match e with 
                    | (Con n, IntT) -> Int n 
                    | (Con 1, BoolT) -> Int 1 
                    | (Con 0, BoolT) -> Int 0 
                    | (Con 0, UnitT) -> Int 0
                    |  _ -> failwith "unknown constant"
    | (EListC, ListT l) -> List []

    | (Var x, htype)  -> lookup env x

    | (Op1 (op, e1), h) -> 
      let v1 = eval e1 env in
      match (op, v1) with
      | ("+", Int v1) -> Int v1
      | ("-", Int v1) -> Int -v1
      |  _ -> failwith "unknown primitive or wrong type"

    | (Op2 (op, e1, e2), h) -> 
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | ("*", Int i1, Int i2) -> Int (i1 * i2)
      | ("/", Int i1, Int i2) -> Int (i1 / i2)
      | ("+", Int i1, Int i2) -> Int (i1 + i2)
      | ("-", Int i1, Int i2) -> Int (i1 - i2)
      | ("=", Int i1, Int i2) -> Int (if i1 = i2 then 1 else 0)
      | ("<", Int i1, Int i2) -> Int (if i1 < i2 then 1 else 0)
      | ("<=", Int i1, Int i2) -> Int (if i1 <= i2 then 1 else 0)
      | (";", Int i1, Int i2) -> Int i2
      | ("::", h, t) -> List [h;t]
      | ("<>", Int i1, Int i2) -> Int (if i1 = i2 then 0 else 1)
      |  _ -> failwith "unknown primitive or wrong type"
      
    | (Let (b, e2), h) -> 
      match (b, e2) with
      | (V (s, e1), e2) -> let v = eval e1 env in
                           let env2 = (s, v) :: env in
                           eval e2 env2
      | (F (f, x, h, e1), e2) -> let env2 = (f, Closure((match f with 
                                                          | "" -> None
                                                          | f -> Some f), string x, e1, env)) :: env in 
                                                          eval e2 env2
 
    | (If (e1, e2, e3), h) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"
  
    | (Lam (t1, e1), h) -> eval e1 env                                   
                                            (* fn (x:t) => e end is encoded as (Lam (("x", t0), e0), t0
                                             e) where t0 encodes the type t of
                                             the input parameter x, e0 encodes the body e of the function, and t0
                                             e encodes the type te of e.*)
  
    | (Call (e1, e2), h) -> 
      let c = eval e1 env
      match c with
      | Closure (f, x, fbody, fenv) ->
        let v = eval e2 env in
        let env1 = (x, v) :: (string f, c) :: fenv in
        eval fbody env1
      | _ -> failwith "eval Call: not a function"

    | (_,_) -> failwith "Unknown expression";;

(* Evaluate in empty environment: program must have no free variables: *)

let run e = eval e []

open Expr
open Rationalsimpl
open Asaeorder
open Asaedef
open Simplification
open Derivative
open Integraltable


let check_integral_table x e = let e = automatic_simplify e in
  List.fold_left
  (fun a b -> match a with
    | Undefined -> b x e
    | _ -> a)
  Undefined
  basic_integral_table

let separate_factors x e =
  let l = (all_operands e) in
  let free_l = List.filter (fun y -> free_of y x) l in
    begin match free_l with
      | [] -> Undefined
      | _ -> automatic_simplify (construct_expr ProdOp free_l)
    end,
    automatic_simplify (construct_expr ProdOp (List.filter (fun y -> not (free_of y x)) l))

let rec function_forms x e = 
  if free_of e x
  then
    []
  else
    let l = List.concat_map (function_forms x) (all_operands e) in
      match (kind e) with
        | FuncOp _ -> e :: l
        | _ -> l

let function_forms_arguments x e =
  List.map (fun y -> (operand y 0)) (function_forms x e)

let app_l x e l =
  List.fold_left (fun a b -> 
    match a with
    | Undefined -> b x e
    | _ -> a )
    Undefined
    l

let rec b_and_e_of_powers x e =
  if free_of e x
  then 
    []
  else
    let l = List.concat_map (b_and_e_of_powers x) (all_operands e) in
      match e with
        | Power(_, _) -> 
          (List.filter (fun y -> not (free_of y x)) (all_operands e)) @ l
        | _ -> l

let sub_candiates x e =
  List.map automatic_simplify (function_forms x e @ function_forms_arguments x e @ b_and_e_of_powers x e)

let diff_sym x = if equal x (Symbol "x") then (Symbol "u") else (Symbol "x")

let rec integrate_rec x e = let e = automatic_simplify e in
  match e with
    | Undefined -> Undefined
    | _ -> app_l x e [check_integral_table; linear_propeteries; substitution_method]
and linear_propeteries x e = match (kind e) with
  | ProdOp ->
    let c, e = separate_factors x e in
    begin match c with
        | Undefined -> Undefined
        | _ -> automatic_simplify (construct_expr ProdOp [c; integrate_rec x e])
      end
  | SumOp -> automatic_simplify (map_fun (integrate_rec x) e)
  | _ -> Undefined

and substitution_method x e =
  let poss_sub = List.filter (fun y -> not (equal y x) && not (equal y undefined)) (sub_candiates x e) in
    List.fold_left
    (fun a b ->
      match a with
      | Undefined -> check_sub x b e
      | _ -> a)
    Undefined
    poss_sub

and check_sub x v e =
  let e_div_dv = automatic_simplify (Quotient(e, derivative x v)) in
    let new_x = (diff_sym x) in
      let u = substitute e_div_dv v new_x in
        if free_of u x
        then
          substitute (integrate_rec new_x u) new_x v
        else
          Undefined


let integrate x e = automatic_simplify (integrate_rec x e)

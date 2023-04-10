open Expr
open Rationalsimpl
open Asaeorder

let cnt_filter p l = List.length (List.filter p l)

let diff_base_rule l = List.for_all (fun x -> cnt_filter (fun y -> equal (base x) (base y)) l = 1) l
let diff_term_rule l = List.for_all (fun x -> cnt_filter (fun y -> equal (term x) (term y)) l = 1) l
let one_const_rule l = (cnt_filter is_const l) <= 1
let rec args_ordered_rule l = match l with
  | [] | _::[]-> true
  | x::y::_ -> compare x y = -1 && args_ordered_rule (List.tl l)

let admissable_factor e = match (kind e) with
  | IntOp -> not (equal e one || equal e zero)
  | FracOp | SymOp | SumOp | PowOp | FactOp | FuncOp _ -> true
  | _ -> false

let admissable_term e = match (kind e) with
  | IntOp -> not (equal e zero)
  | FracOp | SymOp | ProdOp | PowOp | FactOp |FuncOp _ -> true
  | _ -> false


(* this function is in fact the definition of simplified expression *)
let rec is_asae e = match (kind e) with
  | IntOp -> true
  | FracOp -> not (equal (operand e 1) zero || equal (operand e 1) one || equal (operand e 0) zero)
  | SymOp -> true
  | ProdOp -> let l = all_operands e in
              List.for_all is_asae l &&
              List.for_all admissable_factor l &&
              one_const_rule l &&
              diff_base_rule l &&
              args_ordered_rule l
  | SumOp -> let l = all_operands e in
              List.for_all is_asae l &&
              List.for_all admissable_term l &&
              one_const_rule l &&
              diff_term_rule l &&
              args_ordered_rule l
  | PowOp -> let v = (operand e 0) and w = (operand e 1) in
              is_asae v && is_asae w &&
              not (equal w one || equal w zero) &&
              if (kind w) = IntOp
              then
                begin match (kind v) with
                  | SymOp | SumOp | FactOp | FuncOp _ -> true
                  | _ -> false
                end
              else
                not (equal v one || equal v zero)
  | FactOp -> let v = (operand e 0) in
              is_asae v &&
              begin match v with
              | Int n -> I.(compare n zero) < 0
              | _ -> true
              end
  | FuncOp _ -> number_of_operands e > 0 && List.for_all is_asae (all_operands e)
  | _ -> false
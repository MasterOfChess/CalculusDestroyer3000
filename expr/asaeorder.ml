open Expr
open Rationalsimpl

let base e = match (kind e) with
  | SymOp | ProdOp | SumOp | FactOp | FuncOp _ -> e
  | PowOp -> operand e 0
  | _ -> Undefined

let exponent e = match (kind e) with
  | SymOp | ProdOp | SumOp | FactOp | FuncOp _ -> one
  | PowOp -> operand e 1
  | _ -> Undefined

let term e = match (kind e) with
  | SymOp | SumOp | PowOp | FactOp | FuncOp _ -> e
  | ProdOp -> begin match (kind (operand e 0)) with
                | IntOp | FracOp -> let l = List.tl (all_operands e) in
                                      if List.length l = 1
                                      then
                                        List.hd l
                                      else
                                        construct_expr ProdOp l
                | _ -> e
              end
  | _ -> Undefined

let const e = match (kind e) with
  | SymOp | SumOp | PowOp | FactOp | FuncOp _ -> one
  | ProdOp ->
    let f_op = operand e 0 in
      begin match (kind f_op) with
        | IntOp | FracOp -> f_op
        | _ -> one
      end
  | _ -> Undefined


let is_const e = match (kind e) with
  | IntOp | FracOp -> true
  | _ -> false

let const_const u v _cmp = match (to_fraction u), (to_fraction v) with
  | Fraction u, Fraction v -> F.compare u v
  | _ -> failwith "wrong const_const cmp arguments"

let symbol_symbol u v _cmp = String.compare (symbol_name u) (symbol_name v)

let expr_list_cmp l1 l2 cmp = List.compare cmp l1 l2

let arg_list_cmp u v cmp = expr_list_cmp (List.rev (all_operands u)) (List.rev (all_operands v)) cmp

let power_power u v cmp = 
  if equal (base u) (base v)
  then
    cmp (exponent u) (exponent v)
  else
    cmp (base u) (base v)

let fact_fact u v cmp = cmp (operand u 0) (operand v 0)

let func_func u v cmp =
  let c = String.compare (func_name u) (func_name v) in
    if c = 0
    then
      expr_list_cmp (all_operands u) (all_operands v) cmp
    else
      c


let rec compare u v =
  match (kind u), (kind v) with
    | (IntOp|FracOp), (IntOp|FracOp) -> const_const u v compare
    | SymOp, SymOp -> symbol_symbol u v compare
    | ProdOp, ProdOp | SumOp, SumOp -> arg_list_cmp u v compare
    | PowOp, PowOp -> power_power u v compare
    | FactOp, FactOp -> fact_fact u v compare
    | FuncOp _, FuncOp _ -> func_func u v compare
    | IntOp, _ | FracOp, _ -> -1
    | ProdOp, (PowOp|SumOp|FactOp|FuncOp _|SymOp) -> compare u (construct_expr ProdOp [v])
    | PowOp, (SumOp|FactOp|FuncOp _|SymOp) -> compare u (construct_expr PowOp [v; one])
    | SumOp, (FactOp|FuncOp _|SymOp) -> compare u (construct_expr SumOp [v])
    | FactOp, (FuncOp _|SymOp) -> if equal (operand u 0) v
                                  then
                                    1
                                  else
                                    compare u (construct_expr FactOp [v])
    | FuncOp name_u, SymOp -> if String.compare name_u (symbol_name v) = 0
                              then
                                1
                              else
                                compare (symbol [Name(name_u)]) v
    | _ -> -1 * (compare v u)
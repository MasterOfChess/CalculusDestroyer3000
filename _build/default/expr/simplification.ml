open Expr
open Rationalsimpl
open Asaeorder
open Asaedef

let is_undef e = List.exists (equal undefined) (all_operands e)

let rec automatic_simplify u =
  match kind u with
    | IntOp | SymOp -> u
    | FracOp -> simplify_rational_number u
    | _ ->
      let v = construct_expr (kind u) (List.map automatic_simplify (all_operands u)) in
        begin match (kind u) with
          | PowOp -> simplify_power v
          | ProdOp -> simplify_product v
          | SumOp -> simplify_sum v
          | QuotOp -> simplify_quotient v
          | BdiffOp | UdiffOp -> simplify_difference v
          | FactOp -> simplify_factorial v
          | FuncOp _ -> simplify_function v
          | _ -> Undefined
        end
and simplify_power e =
if is_undef e
then
  Undefined
else
  let v = (operand e 0) and w = (operand e 1) in
    if equal v zero
    then
      begin match (kind w) with
        | IntOp | FracOp -> if compare w zero = 1
                            then
                              zero
                            else
                              Undefined
        | _ -> Undefined
      end
    else if equal v one
    then
      one
    else if (kind w) = IntOp
    then
      simplify_integer_power v w
    else
      e

and simplify_integer_power v n =
  if (kind v) = IntOp || (kind v) = FracOp
  then
    simplify_rne (Power(v, n))
  else if equal n zero
  then
    one
  else if equal n one
  then
    v
  else 
      begin match (kind v) with
        | PowOp -> 
            let r = (operand v 0) and s = (operand v 1) in
              let p = simplify_product(construct_expr ProdOp [s; n]) in
                begin match (kind p) with
                  | IntOp -> simplify_integer_power r p
                  | _ -> Power(r, p)
                end
        | ProdOp ->
            let er = map PowOp v [Expr(n)] in
              let args = List.map (fun e -> simplify_integer_power (operand e 0) (operand e 1)) (all_operands er) in
                let r = construct_expr ProdOp args in
                  simplify_product r
        | _ -> Power(v, n)
      end

and simplify_product u =
  if is_undef u
  then
    Undefined
  else
    let l = all_operands u in
      if List.exists (fun x -> equal x zero) l
      then
        zero
      else if List.length l = 1
      then
        List.hd l
      else
        let v = simplify_product_rec l in
          if List.length v = 0
          then
            one
          else if List.length v = 1
          then
            List.hd v
          else
            construct_expr ProdOp v

and simplify_product_rec l =
  if List.length l = 2
  then
    let u1 = List.nth l 0 and u2 = List.nth l 1 in
      begin match (kind u1), (kind u2) with
        | ProdOp, ProdOp -> merge_products (all_operands u1) (all_operands u2)
        | ProdOp, _ -> merge_products (all_operands u1) [u2]
        | _, ProdOp -> merge_products [u1] (all_operands u2)
        | (IntOp|FracOp), (IntOp|FracOp) ->
          let p = simplify_rne (construct_expr ProdOp [u1;u2]) in
            if equal p one
            then
              []
            else
              [p]
        | _ ->  if equal u1 one
                then
                  [u2]
                else if equal u2 one
                then
                  [u1]
                else if equal (base u1) (base u2)
                then
                  let s = simplify_sum (construct_expr SumOp [exponent u1; exponent u2]) in
                    let p = simplify_power (Power(base u1, s)) in
                      if equal p one
                      then
                        []
                      else
                        [p]
                else if compare u2 u1 = -1
                then
                  [u2; u1]
                else
                  l
      end
  else
    let u1 = List.hd l and w = simplify_product_rec (List.tl l) in
      begin match (kind u1) with
        | ProdOp -> merge_products (all_operands u1) w
        | _ -> merge_products [u1] w
      end

and merge_products p q = 
  match p, q with
  | _, [] -> p
  | [], _ -> q
  | p1::pr, q1::qr ->
    let h = simplify_product_rec [p1; q1] in
      begin match h with
        | [] -> merge_products pr qr
        | x::[] -> x::(merge_products pr qr)
        | x::y::_ ->  if equal x p1 && equal y q1
                      then
                        p1::(merge_products pr q)
                      else
                        q1::(merge_products p qr)
      end

and simplify_sum u = 
  if is_undef u
  then
    Undefined
  else
    let l = all_operands u in
      if List.length l = 1
      then
        List.hd l
      else
        let v = simplify_sum_rec l in
          if List.length v = 0
          then
            zero
          else if List.length v = 1
          then
            List.hd v
          else
            construct_expr SumOp v
and simplify_sum_rec l = 
  if List.length l = 2
  then
    let u1 = List.nth l 0 and u2 = List.nth l 1 in
      begin match (kind u1), (kind u2) with
        | SumOp, SumOp -> merge_sums (all_operands u1) (all_operands u2)
        | SumOp, _ -> merge_sums (all_operands u1) [u2]
        | _, SumOp -> merge_sums [u1] (all_operands u2)
        | (IntOp|FracOp), (IntOp|FracOp) ->
          let p = simplify_rne (construct_expr SumOp [u1;u2]) in
            if equal p zero
            then
              []
            else
              [p]
        | _ ->  if equal u1 zero
                then
                  [u2]
                else if equal u2 zero
                then
                  [u1]
                else if equal (term u1) (term u2)
                then
                  let s = simplify_sum (construct_expr SumOp [const u1; const u2]) in
                    let p = simplify_product (construct_expr ProdOp [s; term u1]) in
                      if equal p zero
                      then
                        []
                      else
                        [p]
                else if compare u2 u1 = -1
                then
                  [u2; u1]
                else
                  l
      end
  else
    let u1 = List.hd l and w = simplify_sum_rec (List.tl l) in
      begin match (kind u1) with
        | SumOp -> merge_sums (all_operands u1) w
        | _ -> merge_sums [u1] w
      end

and merge_sums p q = 
  match p, q with
  | _, [] -> p
  | [], _ -> q
  | p1::pr, q1::qr ->
    let h = simplify_sum_rec [p1; q1] in
      begin match h with
        | [] -> merge_sums pr qr
        | x::[] -> x::(merge_sums pr qr)
        | x::y::_ ->  if equal x p1 && equal y q1
                      then
                        p1::(merge_sums pr q)
                      else
                        q1::(merge_sums p qr)
      end

and simplify_quotient e =
  if is_undef e
  then
    Undefined
  else
    let u = (operand e 0) and v = (operand e 1) in
      automatic_simplify (construct_expr ProdOp [u; Power(v, minus_one)])
and simplify_difference e =
  if is_undef e
  then
    Undefined
  else
    match e with
    | Udiff u -> automatic_simplify (construct_expr ProdOp [minus_one; u])
    | Bdiff(u, v) -> automatic_simplify (construct_expr SumOp [u; (construct_expr ProdOp [minus_one; v])])
    | _ -> failwith "simplify_difference wrong argument"

and simplify_factorial e =
  if is_undef e
  then
    Undefined
  else
    match e with
    | Int n ->  if I.(compare n zero) >= 0
                then
                  simplify_rne e
                else 
                  e
    | _ -> e

and simplify_function e =
  if is_undef e
  then
    Undefined
  else
    e







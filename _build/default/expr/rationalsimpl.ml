open Expr


let denom r = match (kind r) with
| IntOp -> Int I.one
| FracOp -> begin match (operand r 1) with
                | Int d  -> Int d
                | _ -> failwith "rationalsimpl denom failure1"
            end
| UndefOp -> Undefined
| _ -> failwith "rationalsimpl denom failure2"

let num r = match (kind r) with
| IntOp -> r
| FracOp -> begin match (operand r 0) with
                | Int n -> Int n
                | _ -> failwith "rationalsimpl num failure1"
                end
| UndefOp -> Undefined
| _ -> failwith "ratonalsimpl num failure2"

let to_fraction r = let n = num r and d = denom r in
  match n, d with
    | Undefined, _ -> Undefined
    | _, Undefined -> Undefined
    | _ -> fraction [Expr(n); Expr(d)]

let simplify_rational_number r = match (kind r) with
  | IntOp -> r
  | FracOp -> 
    let n = (operand r 0) and d = (operand r 1) in
      if equal d (Int I.one)
        then n
      else
        r
  | UndefOp -> Undefined
  | _ -> failwith "wrong simplify_rational_number argument"

let factorial_eval e = match (operand e 0) with
  | Int n -> if I.(compare n zero) < 0
            then
              Undefined
            else
              Int (I.factorial n)
  | Undefined -> Undefined
  | _ -> failwith "rationalsimpl factorial_eval wrong argument"

  
let binop_eval op r1 r2 =
  let r1 = to_fraction r1 and r2 = to_fraction r2 in
  match r1, r2 with
      | Fraction f1, Fraction f2 -> simplify_rational_number (Fraction (op f1 f2))
      | _ -> Undefined

let product_eval = binop_eval F.mul
let sum_eval = binop_eval F.add
let bdiff_eval = binop_eval F.sub
let quotient_eval r1 r2 =
  if equal (num r2) (Int I.zero) then
    Undefined
  else
    binop_eval F.div r1 r2
    
let multop_eval (neutral_element : t) comb_f (arg_list : t list) = List.fold_right comb_f arg_list neutral_element

let rec nonnegative_power_eval f n =
  if I.(equal n zero)
  then
    Int I.one
  else
    let two = I.(nth_int 2) in
      if I.(equal (rem n two) zero)
      then
        let x = nonnegative_power_eval f I.(div n two) in product_eval x x
      else
        product_eval f (nonnegative_power_eval f I.(sub n one))

    
let rec power_eval f n =
  match n with
  | Int n -> 
      if  equal (num f) (Int I.zero)
      then
        if I.(compare n zero) > 0
        then
          Int I.zero
        else
          Undefined
      else
        if I.(compare n zero) >= 0
        then
          nonnegative_power_eval f n
        else
          power_eval (quotient_eval (Int I.one) f) (Int I.(neg n))
  | _ -> Undefined

let neg_eval r =
  match r with
    | Int n -> Int I.(neg n)
    | Fraction f -> Fraction F.(neg f)
    | _ -> Undefined

let rec simplify_rne_rec e =
  let v = match (kind e) with
    | IntOp -> e
    | FracOp -> if equal (denom e) (Int I.zero)
                then
                  Undefined
                else
                  e
    | FactOp -> let n = simplify_rne_rec (operand e 0) in factorial_eval n
    | PowOp -> let f = simplify_rne_rec (operand e 0)
              and n = simplify_rne_rec (operand e 1) in
                  power_eval f n
    | UdiffOp -> let v = simplify_rne_rec (operand e 0) in neg_eval v
    | BdiffOp | QuotOp | ProdOp | SumOp-> let l = List.map simplify_rne_rec (all_operands e) in
        begin match (kind e) with
          | BdiffOp -> multop_eval (Int I.zero) bdiff_eval l
          | QuotOp -> multop_eval (Int I.one) quotient_eval l
          | ProdOp -> multop_eval (Int I.one) product_eval l
          | SumOp -> multop_eval (Int I.zero) sum_eval l
          | _ -> failwith "not happening"
        end
    | _ -> Undefined
  in
    simplify_rational_number v


let simplify_rne e = simplify_rne_rec e
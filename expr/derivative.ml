open Expr
open Rationalsimpl
open Asaeorder
open Asaedef
open Simplification


let ln_fun x = construct (FuncOp "ln") [Expr(x)]
let exp_fun x = construct (FuncOp "exp") [Expr(x)]
let cos_fun x = construct (FuncOp "cos") [Expr(x)]
let sin_fun x = construct (FuncOp "sin") [Expr(x)]
let tan_fun x = construct (FuncOp "tan") [Expr(x)]
let arcsin_fun x = construct (FuncOp "arcsin") [Expr(x)]
let arccos_fun x = construct (FuncOp "arccos") [Expr(x)]
let arctan_fun x = construct (FuncOp "arctan") [Expr(x)]

let two = Int (I.nth_int 2)

let basic_derivatives_aux x e = match func_name e with
  | "exp" -> exp_fun x
  | "ln" -> Power(x, minus_one)
  | "sin" -> cos_fun x
  | "cos" -> Udiff(sin_fun x)
  | "tan" -> Power(cos_fun x, two)
  | "arcsin" -> Quotient(one, Power(Bdiff(one, Power(x, two)), construct_expr FracOp [one; two]))
  | "arccos" -> Udiff(Quotient(one, Power(Bdiff(one, Power(x, two)), construct_expr FracOp [one; two])))
  | "arctan" -> Quotient(one, construct_expr SumOp [one; Power(x, two)])
  | _ -> Undefined

let basic_derivatives x e = automatic_simplify (basic_derivatives_aux x e)


let rec derivative_rec x e = let e = automatic_simplify e in
  if free_of e x
  then
    zero
  else
    match (kind e) with
      | SymOp -> one
      | SumOp -> sum_rule x e
      | ProdOp -> let l = all_operands e in
                    let f = List.hd l
                    and g = automatic_simplify (construct_expr ProdOp (List.tl l)) in
                      product_rule x f g
      | PowOp -> let v = (operand e 0) and w = (operand e 1) in
                  power_rule x v w
      | FuncOp _ -> let arg = (operand e 0) in
                          if equal arg x
                          then
                            basic_derivatives x e
                          else
                            chain_rule x (construct (kind e) [Expr(x)]) arg
      | _ -> Undefined

and sum_rule x e = map_fun (derivative_rec x) e

and product_rule x f g =  
  if free_of f x
  then
    construct_expr ProdOp [f; derivative_rec x g]
  else if free_of g x
  then
    construct_expr ProdOp [g; derivative_rec x f]
  else 
    construct_expr SumOp
    [construct_expr ProdOp [derivative_rec x f; g];
    construct_expr ProdOp [f; derivative_rec x g]]

and chain_rule x h g =
  let h = automatic_simplify h
  and g = automatic_simplify g in
    let dh = derivative_rec x h and dg = derivative_rec x g in
      construct_expr ProdOp [substitute dh x g; dg]

and power_rule x v w =
    if free_of w x (* f(x)^a *)
    then
      if equal v x (* x^a *)
      then
        let new_exponent = construct_expr SumOp [w; minus_one] in
          construct_expr ProdOp [w; Power(x, new_exponent)]
      else
        chain_rule x (Power(x, w)) v
    else if free_of v x (* a^f(x) *)
    then
      if equal w x (* a^x *)
      then
        construct_expr ProdOp [Power(v, x); construct (FuncOp "ln") [Expr(v)]]
      else
        chain_rule x (Power(v, x)) w
    else (* g(x)^f(x) -> exp(ln(g(x))*f(x)) *)
      let new_exponent = construct_expr ProdOp [(construct (FuncOp "ln") [Expr(v)]); w] in
        construct (FuncOp "exp") [Expr(new_exponent)]


let derivative x e = automatic_simplify (derivative_rec x e)
open Expr
open Rationalsimpl
open Asaeorder
(* open Asaedef *)
open Simplification
open Derivative

let rule_strings =
  ["1/x -> ln x"; "x^n -> 1/(n+1)x^(n+1)"; "c -> cx"; "exp x -> exp x"; "ln x -> x ln x - x";
  "sin x -> - cos x"; "cos x -> sin x"; "tan x -> - ln(cos x)"; "arcsin x -> x arcsin x + (1-x^2)^(1/2)";
  "arccos x -> x arccos x - (1-x^2)^(1/2)"; "arctan x -> x arctan x - 1/2*ln(1+x^2)"]


(* x^n -> 1/(n+1)x^(n+1) *)
let rule2 x e =
  let v = base e and w = exponent e in
    if equal v x && free_of w x && not (equal w minus_one)
    then
      let new_exp = construct_expr SumOp [w; one] in
        let c = construct_expr QuotOp [one; new_exp] in
          construct_expr ProdOp [c; Power(x, new_exp)]
    else
      Undefined


(* 1/x -> ln x*)
let rule1 x e = match e with
  | Power(v, w) ->
      if equal w minus_one && equal v x
      then 
        ln_fun x
      else
        Undefined
  | _ -> Undefined

(* c -> cx *)
let rule3 x e = 
  if free_of e x
  then
    construct_expr ProdOp [e; x]
  else
   Undefined


let func_rule s r x e = match (kind e) with
  | FuncOp name ->  if String.equal name s && equal (operand e 0) x
                    then
                      r
                    else
                      Undefined
  | _ -> Undefined

(* exp x -> exp x *)
let rule4 x e = func_rule "exp" e x e

(* ln x -> x ln x - x *)
let rule5 x e =
  func_rule "ln"  (Bdiff(construct_expr ProdOp [x; e], x)) x e

(* sin x -> - cos x *)
let rule6 x e = func_rule "sin" (Udiff (cos_fun x)) x e

(* cos x -> sin x *)
let rule7 x e = func_rule "cos" (sin_fun x) x e

(* tan x -> - ln(cos x) *)
let rule8 x e = func_rule "tan" (Udiff (ln_fun (cos_fun x))) x e

let half = fraction [Expr(one); Expr(two)]
let arc_v x = Power(Bdiff(one, Power(x, two)), half)

(* arcsin x -> x arcsin x + (1-x^2)^(1/2) *)
let rule9 x e =
  func_rule
  "arcsin"
  (construct_expr SumOp [construct_expr ProdOp [x; e]; (arc_v x)])
  x e

(* arccos x -> x arccos x - (1-x^2)^(1/2) *)
let rule10 x e =
  func_rule
  "arccos"
  (Bdiff(construct_expr ProdOp [x; e], (arc_v x)))
  x e

(* arctan x -> x arctan x - 1/2*ln(1+x^2) *)
let rule11 x e =
  func_rule
  "arctan"
  (Bdiff(
    construct_expr ProdOp [x; e],
    construct_expr ProdOp [half; ln_fun (construct_expr SumOp [one; Power(x, two)])]))
  x e


  
let rule_table = [rule1; rule2; rule3; rule4; rule5; rule6; rule7; rule8; rule9; rule10; rule11]


let rec add_debug_aux acc n rt dt =
  if n < 0
  then
    acc
  else
    let f = (fun x e -> let deb = print_endline ( "Rule "^(Int.to_string (n+1))^": "^(List.nth dt n)^" on "^(to_string e)^" with var: "^(to_string x)) in
      let res = (List.nth rt n) x e
      in
      let deb = print_endline ( "Result: "^(to_string res)) in res ) in
      add_debug_aux (f::acc) (n-1) rt dt

let add_debug rt dt = add_debug_aux [] ((List.length rt) - 1) rt dt
(* let basic_integral_table = add_debug rule_table rule_strings *)
let basic_integral_table = rule_table
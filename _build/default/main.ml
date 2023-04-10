open Expression
open Repl

module I = Integers
module F = Fractions
module E = Expr
module RS = Rationalsimpl
module ASO = Asaeorder
module S = Simplification
module D = Derivative

let number n = E.integer [E.IntNum (I.nth_int n)]
let frac_expr a b = E.construct_expr FracOp [number a; number b]
let fact_expr n = E.construct_expr FactOp [number n]
let sin_expr x = E.construct_expr (FuncOp("sin")) [x]
let quot_expr a b = E.construct_expr QuotOp [a; b]
let prod_expr a b = E.construct_expr ProdOp [a; b]
let sum_expr a b = E.construct_expr SumOp [a; b]
let diff_expr a b = E.construct_expr BdiffOp [a; b]
let pow_expr a b = E.construct_expr PowOp [a; b]
let sym_expr s = E.construct SymOp [Name(s)]

let a = sym_expr "a"
let b = sym_expr "b"
let c = sym_expr "c"
let d = sym_expr "d"
let e = sym_expr "e"
let f = sym_expr "f"
let x = sym_expr "x"
let y = sym_expr "y"

let simplify_test_1 = E.construct_expr SumOp [sym_expr "x"; prod_expr (sym_expr "x") (number 2);
prod_expr (sym_expr "y") (pow_expr (sym_expr "y") (number 2)); (pow_expr (sym_expr "z") (number 0));
E.construct_expr (FuncOp("sin")) [quot_expr (E.construct_expr (FuncOp("pi")) []) (number 4)]]

let simplify_test_2 = sum_expr (prod_expr (number 3) (sum_expr x x)) (quot_expr (pow_expr x (number 2)) x)

let simplify_test_3 = quot_expr 
(sum_expr (Udiff b) 
(pow_expr (diff_expr (pow_expr b (number 2)) (prod_expr (number 4) (prod_expr a c))) (quot_expr (number 1) (number 2))))
(prod_expr (number 2) a)

let derivative_test_1 = E.construct_expr SumOp [pow_expr x (number 4); D.sin_fun (pow_expr x (number 2));
diff_expr E.zero (prod_expr (D.ln_fun x) (D.exp_fun x)); number 7]

let print_tree e = E.(e |> tree_to_string) |> print_endline
let print_test s t = s ^ " " ^ E.(t |> to_string) |> print_endline
let print_test_n s t = s ^ " " ^ E.(t |> to_string) |> print_string
let comp_test s e1 e2 = print_test_n "" e1; print_test s e2; print_int (ASO.compare e1 e2); print_endline ""


let _ = print_test "input: " simplify_test_1
(* let _ = print_tree simplify_test_1 *)
let _ = print_test "output simplify: " (S.automatic_simplify simplify_test_1)
(* let _ = print_tree (S.automatic_simplify simplify_test_1) *)
let _ = print_test "input: " simplify_test_2
(* let _ = print_tree simplify_test_2 *)
let _ = print_test "output simplify: " (S.automatic_simplify simplify_test_2)
(* let _ = print_tree (S.automatic_simplify simplify_test_2) *)
let _ = print_test "input: " simplify_test_3
(* let _ = print_tree simplify_test_3 *)
let _ = print_test "output simplify: " (S.automatic_simplify simplify_test_3)
(* let _ = print_tree (S.automatic_simplify simplify_test_3) *)
let _ = print_test "input: " (S.automatic_simplify derivative_test_1)
(* let _ = print_tree simplify_test_3 *)
let _ = print_test "output derivative: " (D.derivative x derivative_test_1)
(* let _ = print_tree (S.automatic_simplify simplify_test_3) *)


let _ = Repl.run_repl ()


open Interp
open Expression

module A = Ast
module E = Expr

let rec ast_to_expr = function
  | A.Var x -> E.Symbol x
  | A.Int n -> E.Int (E.I.nth_int n)
  | A.Fun(name, e) -> E.construct (E.FuncOp name) [E.Expr(ast_to_expr e)]
  | A.Fact e -> E.construct_expr E.FactOp [ast_to_expr e]
  | A.Neg e -> E.construct_expr E.UdiffOp [ast_to_expr e]
  | A.Binop(A.Add, e1, e2) -> E.construct_expr E.SumOp [ast_to_expr e1; ast_to_expr e2]
  | A.Binop(A.Sub, e1, e2) -> E.construct_expr E.BdiffOp [ast_to_expr e1; ast_to_expr e2]
  | A.Binop(A.Div, e1, e2) -> E.construct_expr E.QuotOp [ast_to_expr e1; ast_to_expr e2]
  | A.Binop(A.Mult, e1, e2) -> E.construct_expr E.ProdOp [ast_to_expr e1; ast_to_expr e2]
  | A.Binop(A.Pow, e1, e2) -> E.construct_expr E.PowOp [ast_to_expr e1; ast_to_expr e2]
  | _ -> Undefined

type in_action =
| Close
| Err
| Empty
| Derive of E.t * E.t
| Integrate of E.t * E.t
| Show of E.t
| Simpl of E.t

let parse_line () =
  let l = read_line () in
    if String.equal l String.empty
    then
      Empty
    else
      match Interp.Main.parse l with
        | A.Com(A.Derive(e1, e2)) -> Derive(ast_to_expr e1, ast_to_expr e2)
        | A.Com(A.Integrate(e1, e2)) -> Integrate(ast_to_expr e1, ast_to_expr e2)
        | A.Com(A.Simpl e) -> Simpl (ast_to_expr e)
        | A.Com(A.Repr e) -> Show (ast_to_expr e)
        | A.Com(A.Ext) -> Close
        | _ -> Err

let rec run_repl () =
  match parse_line () with
    | Close -> ()
    | Err -> print_endline "error"; run_repl ()
    | Empty -> run_repl ()
    | Derive(e1, e2) -> print_endline ("> " ^ E.to_string (Derivative.derivative e1 e2));
                        run_repl ()
    | Integrate(e1, e2) -> print_endline ("> " ^ E.to_string (Integrals.integrate e1 e2));
                        run_repl ()
    | Show e -> print_endline ("> " ^ E.tree_to_string e); run_repl ()
    | Simpl e -> print_endline ("> " ^ E.to_string (Simplification.automatic_simplify e)); run_repl ()


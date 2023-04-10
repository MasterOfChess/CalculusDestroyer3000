(** The type of binary operators. *)

type bop = 
  | Add
  | Sub
  | Mult
  | Div
  | Pow

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Binop of bop * expr * expr
  | Com of comd
  | Fun of string * expr
  | Fact of expr
  | Neg of expr
and comd =
  | Derive of expr * expr
  | Integrate of expr * expr
  | Simpl of expr
  | Repr of expr
  | Ext
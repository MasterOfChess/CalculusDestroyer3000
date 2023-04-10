
(* The type of tokens. *)

type token = 
  | WITH
  | TIMES
  | TAN
  | SIN
  | SIMPL
  | RPAREN
  | REPR
  | POWER
  | PLUS
  | OF
  | MINUS
  | LPAREN
  | LN
  | INTGR
  | INT of (int)
  | ID of (string)
  | FACT
  | EXP
  | EXIT
  | EOF
  | DRVT
  | DIV
  | COS
  | ARCTAN
  | ARCSIN
  | ARCCOS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

module L = Exprlist
module I = Integers
module F = Fractions

type optype = 
  | IntOp
  | FracOp
  | SymOp 
  | ProdOp
  | SumOp
  | QuotOp
  | UdiffOp
  | BdiffOp
  | PowOp
  | FactOp
  | FuncOp of string
  | UndefOp

type t =
  | Int of I.t
  | Fraction of F.t
  | Symbol of string
  | Product of t L.t
  | Sum of t L.t
  | Quotient of t * t
  | Udiff of t
  | Bdiff of t * t
  | Power of t * t
  | Factorial of t
  | Func of string * t L.t
  | Undefined

type argtype =
  | IntNum of I.t
  | RatNum of F.t
  | Expr of t
  | Name of string

val integer : argtype list -> t
val fraction : argtype list -> t
val symbol : argtype list -> t
val product : argtype list -> t
val sum : argtype list -> t
val quotient : argtype list -> t
val udiff : argtype list -> t
val bdiff : argtype list -> t
val power : argtype list -> t
val factorial : argtype list -> t
val func : argtype list -> t
val undefined : t

val minus_one : t
val one : t
val zero : t

val kind : t -> optype
val number_of_operands : t -> int
val operand : t -> int -> t
val func_name : t -> string
val symbol_name : t -> string
val unpack_int : t -> I.t
val unpack_frac : t -> F.t
val all_operands : t -> t list
val construct : optype -> argtype list -> t
val construct_expr : optype -> t list -> t
val map : optype -> t -> argtype list -> t
val map_fun : (t -> t) -> t -> t
val equal : t -> t -> bool
val free_of : t -> t -> bool
val substitute : t -> t -> t -> t
val to_string : t -> string
val tree_to_string : t -> string


val ln_fun : t -> t
val exp_fun : t -> t
val cos_fun : t -> t
val sin_fun : t -> t
val tan_fun : t -> t
val arcsin_fun : t -> t
val arccos_fun : t -> t
val arctan_fun : t -> t
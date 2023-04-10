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

let one = Int I.one
let minus_one = Int I.(neg one)
let zero = Int I.zero

module SC = struct
  let get_expr_list n arg_list =
    if List.compare_length_with arg_list n < 0 ||
       not (List.for_all (fun x -> match x with | Expr _ -> true | _ -> false) arg_list)
    then
      failwith "get_expr_list error (constructors)"
    else
      (List.map (fun x -> match x with | Expr y -> y | _ -> failwith "not happening") arg_list)

  let bin_args arg_list =
    let l = get_expr_list 2 arg_list in
      (List.nth l 0), (List.nth l 1)
  
end  

let integer arg_list =
  if List.compare_length_with arg_list 1 < 0
  then
    failwith "integer smart-constructor error"
  else
    match List.nth arg_list 0 with
      | IntNum n -> Int n
      | Expr(Int(n)) -> Int n
      | _ -> failwith "integer smart-constructor error"

let fraction arg_list =
  match List.compare_length_with arg_list 1 with
    | -1 -> failwith "fraction smart-constructor error"
    | 1 -> begin match List.nth arg_list 0, List.nth arg_list 1 with
          | Expr Fraction(f) , _ | RatNum f, _ -> Fraction f
          | Expr Int(n1), Expr(Int(n2))
          | Expr Int(n1), IntNum(n2)
          | IntNum(n1), Expr Int(n2)
          | IntNum(n1), IntNum(n2) -> Fraction (F.fraction n1 n2)
          | _ -> failwith "fraction smart-constructor error"
          end
    | 0 -> begin match List.nth arg_list 0 with
          | Expr Fraction(f) | RatNum f -> Fraction f
          | _ -> failwith "fraction smart-constructor error"
          end
    |_ -> failwith "compare_length_with value not in {-1, 0, 1}"

let symbol arg_list =
  if List.compare_length_with arg_list 0 = 0 then
    failwith "symbol smart-constructor error"
  else
    match List.nth arg_list 0 with
      | Name(name) -> Symbol name
      | _ -> failwith "symbol smart-constructor error"

let func arg_list =
  if List.compare_length_with arg_list 0 = 0 then
    failwith "func smart-constructor error"
  else
    match List.nth arg_list 0 with
    | Name(name) -> let l = SC.get_expr_list 0 (List.tl arg_list) in
                    Func(name, L.from_list l)
    | _ -> failwith "func smart-constructor error"

let product arg_list = Product (L.from_list (SC.get_expr_list 1 arg_list))
let sum arg_list = Sum (L.from_list (SC.get_expr_list 1 arg_list))
let quotient arg_list = let p = SC.bin_args arg_list in Quotient((fst p), (snd p))
let bdiff arg_list = let p = SC.bin_args arg_list in Bdiff((fst p), (snd p))
let power arg_list = let p = SC.bin_args arg_list in Power((fst p), (snd p))

let udiff arg_list = let l = SC.get_expr_list 1 arg_list in Udiff (List.nth l 0)
let factorial arg_list = let l = SC.get_expr_list 1 arg_list in Factorial (List.nth l 0)

let undefined = Undefined

let kind = function
  | Int _ -> IntOp
  | Fraction _ -> FracOp
  | Symbol _ -> SymOp
  | Product _ -> ProdOp
  | Sum _ -> SumOp
  | Quotient _ -> QuotOp
  | Udiff _ -> UdiffOp
  | Bdiff _ -> BdiffOp
  | Power _ -> PowOp
  | Factorial _ -> FactOp
  | Func(name, _) -> FuncOp name
  | Undefined -> UndefOp


let number_of_operands = function
  | Int _ -> 0
  | Fraction _ -> 2
  | Symbol _ -> 0
  | Product l -> L.length l
  | Sum l -> L.length l
  | Quotient _ -> 2
  | Udiff _ -> 1
  | Bdiff _ -> 2
  | Power _ -> 2
  | Factorial _ -> 1
  | Func(_, l) -> L.length l
  | Undefined -> 0

module O = struct
  let int_op e i = match e, i with
    | Int n, 0 -> Int n
    | _, _ -> failwith "int_op argument is non-integral expression"

  let frac_op e i = match e, i with
    | Fraction f, 0 -> Int (F.num f)
    | Fraction f, 1 -> Int (F.denom f)
    | _, _ -> failwith "frac_op argument is non-fraction expression"

  let sym_op e i = match e, i with
    | Symbol _, 0 -> e
    | _, _ -> failwith "sym_op argument is non-symbol expression"

  let un_op e _i = match e with
    | Udiff x | Factorial x -> x
    | _ -> failwith "un_op argument is non-unop expression"

  let bin_op e i = match e with
    | Quotient(x, y) | Bdiff(x, y) | Power(x, y) -> 
      if i = 0 then
        x
      else
        y
    | _ -> failwith "bin_op argument is non-binop expression"
  
  let mult_arg_op e i = 
    match e with
      | (Product l | Sum l) -> L.nth l i
      | _ -> failwith "mult_arg_op argument is non-multi-argument expression"

  let func_op e i =
    match e with
      | Func(_, l) -> L.nth l i
      | _ -> failwith "func_op argument is non-function-argument expression"  
  
  let operand e i =
    if i < 0 || i >= number_of_operands e then
      failwith "operand i argument not in range [0, number_of_operands)"
    else
      match e with
      | Int _ -> int_op e i
      | Fraction _ -> frac_op e i
      | Symbol _ -> sym_op e i
      | Udiff _ | Factorial _ -> un_op e i
      | Quotient _ | Bdiff _ | Power _ -> bin_op e i
      | Product _ | Sum _ -> mult_arg_op e i
      | Func _ -> func_op e i
      | Undefined -> Undefined                 
end

let operand = O.operand

let rec all_operands_aux n acc e =
  if n < 0 then
    acc
  else
    all_operands_aux (n - 1) (operand e n :: acc) e

let all_operands e = all_operands_aux ((number_of_operands e) - 1) [] e


let unpack_frac e = match e with
| Fraction f -> f
| _ -> failwith "Wrong unpack_frac usage"
let unpack_int e = match e with
| Int n -> n
| _ -> failwith "Wrong unpack_int usage"
let func_name e = match e with
| Func(name, _) -> name
| _ -> failwith "Wrong func_name usage"

let symbol_name e = match e with
| Symbol(name) -> name
| _ -> failwith "Wrong symbol_name usage"

let construct op arg_list = match op with
  | IntOp -> integer arg_list
  | FracOp -> fraction arg_list
  | SymOp -> symbol arg_list
  | ProdOp -> product arg_list
  | SumOp -> sum arg_list
  | QuotOp -> quotient arg_list
  | UdiffOp -> udiff arg_list
  | BdiffOp -> bdiff arg_list
  | PowOp -> power arg_list
  | FactOp -> factorial arg_list
  | FuncOp name -> func ((Name name)::arg_list)
  | UndefOp -> undefined

let construct_expr op e_list = construct op (List.map (fun x -> Expr x) e_list)

let map f e args = 
  let l = List.map (fun x -> Expr(construct f (Expr(x)::args))) (all_operands e) in
  match e with
  | Symbol name -> Symbol name
  | _ -> construct (kind e) l

let map_fun f e = match (kind e) with
| SymOp | IntOp | UndefOp -> e
| _ -> construct (kind e) (List.map (fun x -> Expr(f x)) (all_operands e))

let rec equal e1 e2 =
  if kind e1 <> kind e2 then
    false
  else
    match e1, e2 with
      | Int n1, Int n2 -> I.equal n1 n2
      | Fraction f1, Fraction f2 -> F.equal f1 f2
      | Symbol x, Symbol y -> String.equal x y
      | Product l1, Product l2 -> L.length l1 == L.length l2 && L.for_all (fun (x, y) -> equal x y) (L.zip l1 l2)
      | Sum l1, Sum l2 -> L.length l1 = L.length l2 && L.for_all (fun (x, y) -> equal x y) (L.zip l1 l2)
      | Quotient(a, b), Quotient(c, d) -> equal a c && equal b d
      | Udiff x, Udiff y -> equal x y
      | Bdiff(a, b), Bdiff(c, d) -> equal a c && equal b d
      | Power(a, b), Power(c, d) -> equal a c && equal b d
      | Factorial x, Factorial y -> equal x y
      | Func(name1, l1), Func(name2, l2) -> String.equal name1 name2 && L.length l1 = L.length l2 && L.for_all (fun (x, y) -> equal x y) (L.zip l1 l2)
      | Undefined, Undefined -> true
      | _ -> false

let rec free_of e s = not (equal e s) && List.for_all (fun x -> free_of x s) (all_operands e)

let rec substitute e s r =
  if equal e s then
    r
  else match (kind e) with
  | IntOp | SymOp | UndefOp -> e
  | _ ->
    construct
    (kind e)
    (List.map (fun x -> Expr(substitute x s r)) (all_operands e))

let precedence_table = function
  | IntOp -> 0
  | FracOp -> 2
  | SymOp -> 0
  | ProdOp -> 4
  | SumOp -> 6
  | QuotOp -> 3
  | UdiffOp -> 4
  | BdiffOp -> 5
  | PowOp -> 0
  | FactOp -> 2
  | FuncOp _ -> 1
  | UndefOp -> 1

let precedence e = precedence_table (kind e)

let tree_string e = match (kind e) with
  | IntOp -> "Int("^I.(to_string (unpack_int e))^")"
  | FracOp -> "Fraction"
  | SymOp -> symbol_name e
  | ProdOp -> "Product"
  | SumOp -> "Sum"
  | QuotOp -> "Quotient"
  | UdiffOp -> "Udiff"
  | BdiffOp -> "Bdiff"
  | PowOp -> "Power"
  | FactOp -> "Factorial"
  | FuncOp name -> name
  | UndefOp -> "Undefined"

let rec tree_to_string e = 
  let args = String.concat ", " (List.map tree_to_string (all_operands e)) in
    if (kind e) = SymOp || (kind e) = IntOp
    then
      tree_string e
    else
      (tree_string e) ^ "(" ^ args ^ ")"

let rec to_string_aux p e =
  let l = (all_operands e) and pe = (precedence e) in
    let mult_args s pp =
      String.concat s
      (List.map (to_string_aux pp) l)
    in
    let e_s = match e with
              | Int n -> I.to_string n
              | Fraction f -> F.to_string f
              | Symbol name -> name
              | Product _ -> mult_args "*" pe
              | Sum _ -> mult_args "+" pe
              | Quotient _ -> mult_args "/" pe
              | Udiff e -> "-"^(to_string_aux pe e)
              | Bdiff _ -> mult_args "-" pe
              | Power _ -> mult_args "^" pe
              | Factorial e -> (to_string_aux pe e)^"!"
              | Func(name, _) -> let ss = 
                                  mult_args ", " (if number_of_operands e > 1 then 10 else pe)
                                 in
                                 if number_of_operands e > 0 then name^"("^ss^")" else name^" "^ss
              |Undefined -> "Undefined"
    in
    if pe > p then "("^e_s^")" else e_s

let to_string = to_string_aux 10


let ln_fun x = construct (FuncOp "ln") [Expr(x)]
let exp_fun x = construct (FuncOp "exp") [Expr(x)]
let cos_fun x = construct (FuncOp "cos") [Expr(x)]
let sin_fun x = construct (FuncOp "sin") [Expr(x)]
let tan_fun x = construct (FuncOp "tan") [Expr(x)]
let arcsin_fun x = construct (FuncOp "arcsin") [Expr(x)]
let arccos_fun x = construct (FuncOp "arccos") [Expr(x)]
let arctan_fun x = construct (FuncOp "arctan") [Expr(x)]
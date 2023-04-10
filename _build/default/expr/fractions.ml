module I = Integers

type t = Frac of I.t * I.t


let rec normalize_fraction (Frac(a, b)) = match I.(compare b zero) with
  | 0 -> I.(Frac(one, zero))
  | -1 -> normalize_fraction I.(Frac(neg a, neg b))
  | 1 -> let g = I.gcd a b in
    if I.(equal a zero) then 
      I.(Frac(zero, one))
    else
      I.(Frac(a / g, b / g))
  | _ -> failwith "Integer comparison result not in {-1, 0, 1}"

let fraction a b = normalize_fraction (Frac(a, b))
let int_to_fraction n = Frac(n, I.one)

let one = int_to_fraction I.one
let zero = int_to_fraction I.zero

let num (Frac(a, _)) = a
let denom (Frac(_, b)) = b

let neg (Frac(a, b)) = normalize_fraction I.(Frac(neg a, b))

let inv (Frac(a, b)) = normalize_fraction (Frac(b, a))

let add (Frac(a, b)) (Frac(c, d)) =
  if (I.compare b d) = 0 then
    normalize_fraction I.(Frac(a + c, b))
  else
    normalize_fraction I.(Frac(a * d + c * b, b * d))

let sub f (Frac(c, d)) = add f I.(Frac(neg c, d))

let mul (Frac(a, b)) (Frac(c, d)) = normalize_fraction I.(Frac(a * c, b * d))

let div f1 f2 = mul f1 (inv f2)

let to_string (Frac(a, b)) = (I.to_string a) ^ "/" ^ (I.to_string b)

let compare (Frac(a, b)) (Frac(c, d)) = let sign_comp = I.(compare (a * b) (b * c)) in
    if sign_comp <> 0 then
      sign_comp
    else
      I.(compare (a * d) (b * c))

let equal f1 f2 = compare f1 f2 = 0

let power f n = 
  if I.(equal n zero) then
    one
  else
    fraction (I.power (num f) n) (I.power (denom f) n)
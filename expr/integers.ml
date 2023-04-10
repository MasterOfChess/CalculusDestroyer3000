type t = int

let zero = 0
let one = 1
let minus_one = -1
let nth_int (n : int) : t = n

let neg = Int.neg
let add = Int.add
let sub = Int.sub
let mul = Int.mul
let div = Int.div
let rem = Int.rem
let abs = Int.abs
let equal = Int.equal
let compare = Int.compare
let min = Int.min
let max = Int.max
let rec gcd a b = if (equal b zero) then abs a else gcd b (rem a b)
let rec power a n =
  if equal n zero then
    one
  else
    let two = nth_int 2 in
      if equal (rem n two) zero then
        let x = power a (div n two) in mul x x
      else
        mul (power a (sub n one)) a

let rec factorial n =
  if equal n zero then
    one
  else
    mul n (factorial (sub n one))

let ( + ) = ( + )
let ( * ) = ( * )
let ( / ) = ( / )
let ( mod ) = ( mod )

let to_string = Int.to_string
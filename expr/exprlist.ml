type 'a t = 'a list

let empty = []

let length = List.length

let cons = List.cons

let hd = List.hd

let tl = List.tl

let nth = List.nth

let rec from_list xs = match xs with
  | [] -> empty
  | x::xs -> cons x (from_list xs)

let to_list = Fun.id

let zip = List.combine
let for_all = List.for_all

let compare_length_with = List.compare_length_with
let is_empty l = compare_length_with l 0 = 0

let map = List.map
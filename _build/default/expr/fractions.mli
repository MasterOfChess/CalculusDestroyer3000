type t

module I = Integers

val normalize_fraction : t -> t
val fraction : I.t -> I.t -> t
val int_to_fraction : I.t -> t
val one : t
val zero : t
val num : t -> I.t
val denom : t -> I.t
val neg : t -> t
val inv : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val to_string : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val power : t -> I.t -> t
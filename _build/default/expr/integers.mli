type t

val zero : t
val one : t
val minus_one : t
val nth_int : int -> t

val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val rem : t -> t -> t
val abs : t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t
val gcd : t -> t -> t
val power : t -> t -> t
val factorial : t -> t

val ( + ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( mod ) : t -> t -> t

val to_string : t -> string
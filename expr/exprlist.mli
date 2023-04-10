type 'a t

val empty : 'a t

val length : 'a t -> int

val cons : 'a -> 'a t -> 'a t

val hd : 'a t -> 'a

val tl : 'a t -> 'a t

val nth : 'a t -> int -> 'a

val from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val zip : 'a t -> 'b t -> ('a * 'b) t
val for_all : ('a -> bool) -> 'a t -> bool
val compare_length_with : 'a t -> int -> int
val is_empty : 'a t -> bool
val map : ('a -> 'b) -> 'a t -> 'b t
open! Core
include module type of Q with type t = Q.t

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
val hash : t -> int

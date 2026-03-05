open! Core
include Q

let sexp_of_t q = String.sexp_of_t (Q.to_string q)
let t_of_sexp sexp = Q.of_string (String.t_of_sexp sexp)
let hash q = String.hash (Q.to_string q)

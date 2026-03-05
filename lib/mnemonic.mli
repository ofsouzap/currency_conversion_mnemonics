open! Core

module Step : sig
  type t [@@deriving sexp]

  val to_string : t -> string
  val score : t -> int
  val apply : t -> Q.t -> Q.t
  val all : t list
end

type t [@@deriving sexp]

(** The correct exchange rate that was queried for *)
val correct_rate : t -> Q.t

(** The steps taken to arrive at the correct rate *)
val steps : t -> Step.t list

(** The exchange rate calculated by the mnemonic *)
val calculated_rate : t -> Q.t

(** The error between the calculated rate and the correct rate *)
val error : t -> Q.t

val make : correct_rate:Q.t -> steps:Step.t list -> t

open! Core

module Step = struct
  type t =
    | Identity
    | Times_two
    | Times_three
    | Times_five
    | Times_ten
    | Divide_two
    | Divide_five
    | Divide_ten
  [@@deriving sexp, enumerate]

  (* TODO - load scores from a config file instead of being hard-coded *)

  let to_string = function
    | Identity -> "×1"
    | Times_two -> "×2"
    | Times_three -> "×3"
    | Times_five -> "×5"
    | Times_ten -> "×10"
    | Divide_two -> "÷2"
    | Divide_five -> "÷5"
    | Divide_ten -> "÷10"

  let score = function
    | Identity -> 0
    | Times_two -> 2
    | Times_three -> 8
    | Times_five -> 8
    | Times_ten -> 1
    | Divide_two -> 4
    | Divide_five -> 5
    | Divide_ten -> 2

  let apply step rate =
    let times n = Q.(rate * of_int n) in
    let divide n = Q.(rate / of_int n) in
    match step with
    | Identity -> rate
    | Times_two -> times 2
    | Times_three -> times 3
    | Times_five -> times 5
    | Times_ten -> times 10
    | Divide_two -> divide 2
    | Divide_five -> divide 5
    | Divide_ten -> divide 10
end

(* TODO - consider having lazy expressions for calculated rate and error *)

type t = { correct_rate : Q_with_sexp.t; steps : Step.t list }
[@@deriving sexp, fields]

let calculated_rate { correct_rate; steps } =
  List.fold steps ~init:Q.one ~f:(fun acc step -> Step.apply step acc)

let error { correct_rate; steps } =
  let calculated_rate = calculated_rate { correct_rate; steps } in
  Q.(abs (calculated_rate - correct_rate) / correct_rate)

let make ~correct_rate ~steps = { correct_rate; steps }

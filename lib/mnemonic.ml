open! Core

module Step = struct
  type t = Times of int | Divide of int [@@deriving sexp]

  (* TODO - load scores from a config file instead of being hard-coded *)

  let score = function
    | Times 1 -> 0
    | Times 2 -> 2
    | Times 10 -> 1
    | Times _ -> 8
    | Divide 1 -> 0
    | Divide 2 -> 4
    | Divide 10 -> 2
    | Divide 5 -> 5
    | Divide _ -> 15

  let apply step rate =
    match step with
    | Times n -> Q.(rate * of_int n)
    | Divide n -> Q.(rate / of_int n)

  let all () =
    [
      Times 2;
      Times 3;
      Times 5;
      Times 10;
      Divide 2;
      Divide 3;
      Divide 5;
      Divide 10;
    ]
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

open! Core

module type Graph = sig
  module Cost : sig
    type t

    include Comparable.S with type t := t
    include Container.Summable with type t := t
  end

  module State : Hashtbl.Key

  module Transition : sig
    type t

    val to_ : t -> State.t
    val cost : t -> Cost.t
  end

  val initial_state : State.t
  val transitions : State.t -> Transition.t list
end

module type Shortest_paths = sig
  module G : Graph

  type t

  val to_list : t -> (G.State.t * G.Transition.t list) list
  val get : t -> G.State.t -> G.Transition.t list option
end

module type S = sig
  module Make (G : Graph) : sig
    module Shortest_paths : Shortest_paths with module G := G

    val compute_shortest_paths :
      maximum_path_length_incl:int -> unit -> Shortest_paths.t
  end
end

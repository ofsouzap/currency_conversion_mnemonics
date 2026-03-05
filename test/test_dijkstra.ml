open! Core
open! Currency_conversion_mnemonics

(* Test module for Dijkstra algorithm *)

module Simple_graph = struct
  module Cost = Int

  module State = struct
    include String

    let all = [ "start"; "A"; "B"; "C"; "D"; "E" ]
  end

  module Transition = struct
    type t = { to_ : State.t; cost : Cost.t } [@@deriving sexp]

    let to_ t = t.to_
    let cost t = t.cost
  end

  let initial_state = "start"

  let transitions (state : State.t) : Transition.t list =
    (* Graph structure:
     *
     *   start --(1)--> A --(2)--> C --(1)--> E
     *     |                        ^         ^
     *     |                        |         |
     *     +---(5)--> B --(2)-------+         |
     *                |                       |
     *                +---(1)--> D --(1)------+
     *
     * Multiple paths to E:
     *   - start -> A -> C -> E  (cost: 4)
     *   - start -> B -> C -> E  (cost: 8)
     *   - start -> B -> D -> E  (cost: 7)
     * Shortest path to E should be through A and C.
     *)
    match state with
    | "start" -> [ { to_ = "A"; cost = 1 }; { to_ = "B"; cost = 5 } ]
    | "A" -> [ { to_ = "C"; cost = 2 } ]
    | "B" -> [ { to_ = "C"; cost = 2 }; { to_ = "D"; cost = 1 } ]
    | "C" -> [ { to_ = "E"; cost = 1 } ]
    | "D" -> [ { to_ = "E"; cost = 1 } ]
    | "E" -> []
    | _ -> []
end

module Dijkstra_simple = Dijkstra.Make (Simple_graph)

(* Helper function to print shortest paths *)
let print_shortest_paths ~states ~get_path ~sexp_of_transition_list ~cost =
  List.iter states ~f:(fun state ->
      match get_path state with
      | None -> printf "%s: unreachable\n" state
      | Some path ->
          let path_sexp = sexp_of_transition_list path in
          let total_cost = List.sum (module Int) ~f:cost path in
          printf "%s (cost %d): %s\n" state total_cost
            (Sexp.to_string_hum path_sexp))

let%expect_test "shortest paths in simple graph" =
  let paths =
    Dijkstra_simple.compute_shortest_paths ~maximum_path_length_incl:10 ()
  in
  print_shortest_paths ~states:Simple_graph.State.all
    ~get_path:(Dijkstra_simple.Shortest_paths.get paths)
    ~sexp_of_transition_list:[%sexp_of: Simple_graph.Transition.t list]
    ~cost:Simple_graph.Transition.cost;
  [%expect
    {|
    start (cost 0): ()
    A (cost 1): (((to_ A) (cost 1)))
    B (cost 5): (((to_ B) (cost 5)))
    C (cost 3): (((to_ A) (cost 1)) ((to_ C) (cost 2)))
    D (cost 6): (((to_ B) (cost 5)) ((to_ D) (cost 1)))
    E (cost 4): (((to_ A) (cost 1)) ((to_ C) (cost 2)) ((to_ E) (cost 1)))
    |}]

(* Two-state graph *)
module Two_state_graph = struct
  module Cost = Int

  module State = struct
    include String

    let all = [ "start"; "end" ]
  end

  module Transition = struct
    type t = { to_ : State.t; cost : Cost.t } [@@deriving sexp]

    let to_ t = t.to_
    let cost t = t.cost
  end

  let initial_state = "start"

  let transitions (state : State.t) : Transition.t list =
    (* Graph structure:
     *
     *   start --(3)--> end
     *)
    match state with
    | "start" -> [ { to_ = "end"; cost = 3 } ]
    | "end" -> []
    | _ -> []
end

module Dijkstra_two_state = Dijkstra.Make (Two_state_graph)

let%expect_test "two state graph" =
  let paths =
    Dijkstra_two_state.compute_shortest_paths ~maximum_path_length_incl:10 ()
  in
  print_shortest_paths ~states:Two_state_graph.State.all
    ~get_path:(Dijkstra_two_state.Shortest_paths.get paths)
    ~sexp_of_transition_list:[%sexp_of: Two_state_graph.Transition.t list]
    ~cost:Two_state_graph.Transition.cost;
  [%expect
    {|
    start (cost 0): ()
    end (cost 3): (((to_ end) (cost 3)))
    |}]

let%expect_test "maximum path length enforced" =
  (* Using Simple_graph but limiting path length to 2 *)
  let paths =
    Dijkstra_simple.compute_shortest_paths ~maximum_path_length_incl:2 ()
  in
  print_shortest_paths ~states:Simple_graph.State.all
    ~get_path:(Dijkstra_simple.Shortest_paths.get paths)
    ~sexp_of_transition_list:[%sexp_of: Simple_graph.Transition.t list]
    ~cost:Simple_graph.Transition.cost;
  [%expect
    {|
    start (cost 0): ()
    A (cost 1): (((to_ A) (cost 1)))
    B (cost 5): (((to_ B) (cost 5)))
    C (cost 3): (((to_ A) (cost 1)) ((to_ C) (cost 2)))
    D (cost 6): (((to_ B) (cost 5)) ((to_ D) (cost 1)))
    E: unreachable
    |}]

(* Linear chain graph *)
module Linear_chain_graph = struct
  module Cost = Int

  module State = struct
    include String

    let all = [ "start"; "s1"; "s2"; "s3" ]
  end

  module Transition = struct
    type t = { to_ : State.t; cost : Cost.t } [@@deriving sexp]

    let to_ t = t.to_
    let cost t = t.cost
  end

  let initial_state = "start"

  let transitions (state : State.t) : Transition.t list =
    (* Graph structure:
     *
     *   start --(1)--> s1 --(2)--> s2 --(3)--> s3
     *)
    match state with
    | "start" -> [ { to_ = "s1"; cost = 1 } ]
    | "s1" -> [ { to_ = "s2"; cost = 2 } ]
    | "s2" -> [ { to_ = "s3"; cost = 3 } ]
    | "s3" -> []
    | _ -> []
end

module Dijkstra_linear_chain = Dijkstra.Make (Linear_chain_graph)

let%expect_test "linear chain graph" =
  let paths =
    Dijkstra_linear_chain.compute_shortest_paths ~maximum_path_length_incl:10 ()
  in
  print_shortest_paths ~states:Linear_chain_graph.State.all
    ~get_path:(Dijkstra_linear_chain.Shortest_paths.get paths)
    ~sexp_of_transition_list:[%sexp_of: Linear_chain_graph.Transition.t list]
    ~cost:Linear_chain_graph.Transition.cost;
  [%expect
    {|
    start (cost 0): ()
    s1 (cost 1): (((to_ s1) (cost 1)))
    s2 (cost 3): (((to_ s1) (cost 1)) ((to_ s2) (cost 2)))
    s3 (cost 6): (((to_ s1) (cost 1)) ((to_ s2) (cost 2)) ((to_ s3) (cost 3)))
    |}]

(* Triangle graph - tests that better paths overwrite worse ones *)
module Triangle_graph = struct
  module Cost = Int

  module State = struct
    include String

    let all = [ "start"; "A"; "B" ]
  end

  module Transition = struct
    type t = { to_ : State.t; cost : Cost.t } [@@deriving sexp]

    let to_ t = t.to_
    let cost t = t.cost
  end

  let initial_state = "start"

  let transitions (state : State.t) : Transition.t list =
    (* Graph structure (undirected):
     *
     *      start
     *       /  \
     *    (1)    (10)
     *     /      \
     *    A ---(2)- B
     *
     * Direct path to B costs 10, but path via A costs 1+2=3.
     * This tests that when we add B to the queue with cost 10,
     * but then later add it with cost 3, the better path wins.
     *)
    match state with
    | "start" -> [ { to_ = "A"; cost = 1 }; { to_ = "B"; cost = 10 } ]
    | "A" -> [ { to_ = "start"; cost = 1 }; { to_ = "B"; cost = 2 } ]
    | "B" -> [ { to_ = "start"; cost = 10 }; { to_ = "A"; cost = 2 } ]
    | _ -> []
end

module Dijkstra_triangle = Dijkstra.Make (Triangle_graph)

let%expect_test "triangle graph - better path overwrites worse" =
  let paths =
    Dijkstra_triangle.compute_shortest_paths ~maximum_path_length_incl:10 ()
  in
  print_shortest_paths ~states:Triangle_graph.State.all
    ~get_path:(Dijkstra_triangle.Shortest_paths.get paths)
    ~sexp_of_transition_list:[%sexp_of: Triangle_graph.Transition.t list]
    ~cost:Triangle_graph.Transition.cost;
  [%expect {|
    start (cost 0): ()
    A (cost 1): (((to_ A) (cost 1)))
    B (cost 3): (((to_ A) (cost 1)) ((to_ B) (cost 2)))
    |}]

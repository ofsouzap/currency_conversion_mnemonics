open! Core
open Dijkstra_intf

module Make (G : Graph) = struct
  module Path = struct
    type t = G.Transition.t list

    let cost = List.sum (module G.Cost) ~f:G.Transition.cost
    let length = List.length
  end

  module Priority_queue = struct
    module Entry = struct
      type t = { state : G.State.t; path : Path.t }

      let compare { path = path_a; _ } { path = path_b; _ } =
        G.Cost.compare (Path.cost path_a) (Path.cost path_b)
    end

    type t = Entry.t list

    let make () = []
    let add t entry = List.sort (entry :: t) ~compare:Entry.compare

    let dequeue = function
      | [] -> None
      | hd :: tl -> Some (hd, tl)
  end

  module Shortest_paths = struct
    type t = (G.State.t, Path.t) Hashtbl.t

    let to_list = Hashtbl.to_alist
    let get = Hashtbl.find
  end

  (** Compute the shortest paths to each state in a state space *)
  let compute_shortest_paths ~maximum_path_length_incl () =
    let best_paths = Hashtbl.create (module G.State) in

    let rec perform_search ~queue () =
      match Priority_queue.dequeue queue with
      | None -> ()
      | Some ({ Priority_queue.Entry.state; path }, queue) -> (
          let continue () = perform_search ~queue () in
          let continue_with ~new_queue () =
            perform_search ~queue:new_queue ()
          in

          if Path.length path > maximum_path_length_incl then
            (* Path is too long, skip entirely *)
            continue ()
          else
            match Hashtbl.find best_paths state with
            | Some existing_best_path
              when G.Cost.(Path.(cost existing_best_path <= cost path)) ->
                (* Already have a path that's as good or better, skip *)
                continue ()
            | Some _ | None ->
                (* This is a new best path for this state *)
                Hashtbl.set best_paths ~key:state ~data:path;

                let new_entries =
                  G.transitions state
                  |> List.map ~f:(fun transition ->
                      let to_state = G.Transition.to_ transition in
                      let new_path = path @ [ transition ] in
                      Priority_queue.Entry.{ state = to_state; path = new_path })
                in
                let new_queue =
                  List.fold new_entries ~init:queue ~f:Priority_queue.add
                in
                continue_with ~new_queue ())
    in

    perform_search
      ~queue:
        Priority_queue.(add (make ()) { state = G.initial_state; path = [] })
      ();

    best_paths
end

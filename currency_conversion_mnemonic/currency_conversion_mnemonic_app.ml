open! Core
module Algorithm = Currency_conversion_mnemonics.Algorithm
module Mnemonic = Currency_conversion_mnemonics.Mnemonic

module App : Quickterface.App.S =
functor
  (Io : Quickterface.Io.S)
  ->
  struct
    let main_result ~io () =
      let open Result.Let_syntax in
      let%lwt correct_rate_string =
        Io.input_text io ~prompt:"Enter currency conversion rate (as number): "
          ()
      in
      match Q.of_string correct_rate_string with
      | exception exn -> Lwt.return (Error Error.(to_string_hum (of_exn exn)))
      | correct_rate -> (
          match Algorithm.calculate ~correct_rate () with
          | Error message -> Lwt.return (Error message)
          | Ok mnemonic ->
              let output_string =
                let steps =
                  [ "1" ]
                  @ List.map (Mnemonic.steps mnemonic) ~f:(fun step ->
                      sprintf "[%s]" (Mnemonic.Step.to_string step))
                  @ [
                      Mnemonic.calculated_rate mnemonic
                      |> Q.to_float
                      |> Float.round_decimal ~decimal_digits:3
                      |> Float.to_string_hum;
                    ]
                in
                String.concat ~sep:" -> " steps
              in
              let%lwt () = Io.output_text io output_string () in
              Lwt.return (Ok ()))

    let main ~io () =
      let open Lwt.Let_syntax in
      match%lwt main_result ~io () with
      | Ok () -> Lwt.return ()
      | Error message ->
          let%lwt () = Io.output_text io message () in
          Lwt.return ()
  end

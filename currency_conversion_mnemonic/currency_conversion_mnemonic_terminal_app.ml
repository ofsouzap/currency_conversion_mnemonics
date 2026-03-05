module Terminal_app =
  Quickterface_terminal_app.Terminal_app_intf.Make (Currency_conversion_mnemonic_app.App)

let () = Terminal_app.command ~argv:Sys.argv ()

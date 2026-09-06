(** The [caravan] executable.  Everything lives in [Caravan_cli] so it can
    be tested; this is only the entry point. *)

let () = Caravan_cli.Cli.main ()

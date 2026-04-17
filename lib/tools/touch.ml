open OrchCaml

module Touch : Tool.TOOL with type input = string and type output = (unit, string) result = struct
  let name = "touch"
  let description = "Creates an empty file or updates its modification time."
  type input = string
  type output = (unit, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The file path to touch."
        ]
      ];
      "required", `List [`String "path"]
    ]

  let parse_args json =
    let open Yojson.Safe.Util in
    try Ok (json |> member "path" |> to_string)
    with Type_error (s, _) -> Error s

  let format_output = function Ok () -> "File touched successfully." | Error e -> e

  type _ Effect.t += Exec : input -> output Effect.t

  let execute path =
    try
      let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
      Unix.close fd;
      let time = Unix.time () in
      Unix.utimes path time time;
      Ok ()
    with Unix.Unix_error (err, _, _) -> Error ("Touch failed: " ^ Unix.error_message err)
end

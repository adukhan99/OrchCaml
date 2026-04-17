open OrchCaml

module Mkdir : Tool.TOOL with type input = string and type output = (unit, string) result = struct
  let name = "mkdir"
  let description = "Creates a directory including any necessary parent directories."
  type input = string
  type output = (unit, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The directory path to create."
        ]
      ];
      "required", `List [`String "path"]
    ]

  let parse_args json =
    let open Yojson.Safe.Util in
    try Ok (json |> member "path" |> to_string)
    with Type_error (s, _) -> Error s

  let format_output = function Ok () -> "Directory created successfully." | Error e -> e

  type _ Effect.t += Exec : input -> output Effect.t

  let rec mkdir_p path =
    if Sys.file_exists path then ()
    else
      let parent = Filename.dirname path in
      if parent <> path then mkdir_p parent;
      Unix.mkdir path 0o755

  let execute path =
    try
      mkdir_p path;
      Ok ()
    with Unix.Unix_error (err, _, _) -> Error ("Mkdir failed: " ^ Unix.error_message err)
end

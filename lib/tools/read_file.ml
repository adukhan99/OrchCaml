open OrchCaml

module Read_file : Tool.TOOL with type input = string and type output = (string, string) result = struct
  let name = "read_file"
  let description = "Reads the entire contents of a file."
  type input = string
  type output = (string, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The file path to read from."
        ]
      ];
      "required", `List [`String "path"]
    ]

  type _ Effect.t += Exec : input -> output Effect.t

  let execute path =
    try
      let ch = open_in path in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      Ok s
    with Sys_error msg -> Error ("Failed to read file: " ^ msg)
end

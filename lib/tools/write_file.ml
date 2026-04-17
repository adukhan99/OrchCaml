open OrchCaml

type write_input = { path : string; content : string }

module Write_file : Tool.TOOL with type input = write_input and type output = (unit, string) result = struct
  let name = "write_file"
  let description = "Writes content to a file, overwriting if it exists."
  type input = write_input
  type output = (unit, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The file path to write to."
        ];
        "content", `Assoc [
          "type", `String "string";
          "description", `String "The content to write."
        ]
      ];
      "required", `List [`String "path"; `String "content"]
    ]

  type _ Effect.t += Exec : input -> output Effect.t

  let execute { path; content } =
    try
      let ch = open_out path in
      output_string ch content;
      close_out ch;
      Ok ()
    with Sys_error msg -> Error ("Failed to write file: " ^ msg)
end

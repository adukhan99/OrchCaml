open OrchCaml

type grep_input = {
  pattern : string;
  path : string;
}

module Grep : Tool.TOOL with type input = grep_input and type output = (string, string) result = struct
  let name = "grep"
  let description = "Search for a pattern in a file (basic grep)."
  type input = grep_input
  type output = (string, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "pattern", `Assoc [
          "type", `String "string";
          "description", `String "The regex pattern to search for."
        ];
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The file path to search in."
        ]
      ];
      "required", `List [`String "pattern"; `String "path"]
    ]

  type _ Effect.t += Exec : input -> output Effect.t

  let execute { pattern; path } =
    try
      let rex = Re.Pcre.regexp pattern in
      let ch = open_in path in
      let rec loop acc =
        try
          let line = input_line ch in
          if Re.execp rex line then loop (line :: acc) else loop acc
        with End_of_file -> List.rev acc
      in
      let lines = loop [] in
      close_in ch;
      Ok (String.concat "\n" lines)
    with 
    | Sys_error msg -> Error ("Grep failed: " ^ msg)
    | Re.Pcre.Parse_error -> Error "Grep failed: invalid regex pattern"
end

open OrchCaml

type sed_input = {
  path : string;
  pattern : string;
  replacement : string;
}

module Sed : Tool.TOOL with type input = sed_input and type output = (unit, string) result = struct
  let name = "sed"
  let description = "Basic sed replacement in a file (replaces all occurrences of a regex pattern with a replacement string)."
  type input = sed_input
  type output = (unit, string) result

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "path", `Assoc [
          "type", `String "string";
          "description", `String "The file to execute sed on."
        ];
        "pattern", `Assoc [
          "type", `String "string";
          "description", `String "The string regex pattern to look for."
        ];
        "replacement", `Assoc [
          "type", `String "string";
          "description", `String "The replacement string."
        ]
      ];
      "required", `List [`String "path"; `String "pattern"; `String "replacement"]
    ]

  type _ Effect.t += Exec : input -> output Effect.t

  let execute { path; pattern; replacement } =
    try
      let ch = open_in path in
      let content = really_input_string ch (in_channel_length ch) in
      close_in ch;
      
      let rex = Re.Pcre.regexp pattern in
      let new_content = Re.replace_string rex ~by:replacement content in

      let ch_out = open_out path in
      output_string ch_out new_content;
      close_out ch_out;
      Ok ()
    with 
    | Sys_error msg -> Error ("Sed failed: " ^ msg)
    | Re.Pcre.Parse_error -> Error "Sed failed: invalid regex pattern"
end

let parse_row r =
  String.split_on_char ',' r
  |> String.concat "\o033[31m,\o033[0m "
  |> Printf.sprintf "\o033[1;34m>\o033[0m \o033[31m[\o033[0m %s \o033[31m]\o033[0m"
  |> print_endline
;;

(* Read the whole file in memory *)
(* let () = In_channel.input_all In_channel.stdin |> parse_row *)

(* Read the file line by line *)
let () =
  let ic = In_channel.stdin in
  let rec parse infile =
    match In_channel.input_line infile with
    | Some row ->
      parse_row row;
      parse infile
    | None -> ()
  in
  parse ic
;;

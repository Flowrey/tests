type document
type element

external document : document = "document"
external get_by_id : document -> string -> element = "getElementById" [@@mel.send]
external get_class_name : element -> string = "className" [@@mel.get]
external set_inner_html : element -> string -> unit = "innerHTML" [@@mel.set]
external set_class_name : element -> string -> unit = "className" [@@mel.set]
external create_element : document -> string -> element = "createElement" [@@mel.send]
external append_child : element -> element -> unit = "appendChild" [@@mel.send]
external set_attribute : element -> string -> string -> unit = "setAttribute" [@@mel.send]
external add_event_listener : element -> string -> (unit -> unit) -> unit = "addEventListener" [@@mel.send]


(* Teaching Unit to DOM Element *)
let tu_to_element (teaching_unit : Lib.teaching_unit) =
  let div = create_element document "div" in
  let () = set_class_name div "teaching-unit" in
  let button = create_element document "button" in
  let () = add_event_listener button "click" (fun _e -> 
    match get_class_name div with
    | "teaching-unit" -> div |. set_class_name "teaching-unit selected"
    | "teaching-unit selected" -> div |. set_class_name "teaching-unit validated"
    | "teaching-unit validated" -> div |. set_class_name "teaching-unit"
    | _ -> ()
  ) in
  let () = set_inner_html button teaching_unit.code in
  let () = append_child div button in
  div
;;

(* Unit Block to DOM Element *)
let rec ub_to_element (unit_block : Lib.unit_block) =
  let div = create_element document "div" in
  let () = set_class_name div "unit-block" in
  let () = unit_block.units |> List.map tu_to_element |> List.iter (append_child div) in
  let () =
    match unit_block.children with
    | [] -> ()
    | children ->
      let () = set_class_name div "unit-block parent" in
      children |> List.map ub_to_element |> List.iter (append_child div)
  in
  div
;;

(* Display DOM for LG02501A *)
let degree = Lib._LG02501A in
let root = get_by_id document "root" in
let title = create_element document "h1" in
let () = set_inner_html title degree.name in
let () = append_child root title in
degree.curriculum |> List.map ub_to_element |> List.iter (append_child root)

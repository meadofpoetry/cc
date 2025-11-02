

type t = string
[@@deriving show]

let temp =
  let count = ref (-1) in
  fun () -> incr count; Printf.sprintf "tmp.%d" !count

let rec label =
  let table = Hashtbl.create 100 in
  fun string ->
  match Hashtbl.find_opt table string with
  | Some count ->
     incr count;
     Printf.sprintf "%s.%d" string !count
  | None ->
     Hashtbl.add table string (ref (-1));
     label string

let rec var =
  let table = Hashtbl.create 100 in
  fun string ->
  match Hashtbl.find_opt table string with
  | Some count ->
     incr count;
     Printf.sprintf "%s_%d" string !count
  | None ->
     Hashtbl.add table string (ref (-1));
     var string

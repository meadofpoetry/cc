

type t = string
[@@deriving show]

let temp =
  let count = ref (-1) in
  fun () -> incr count; Printf.sprintf "tmp.%d" !count

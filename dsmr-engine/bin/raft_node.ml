open Core

let node_id = ref (-1)
let port = ref 0
let peer_list_string = ref ""
  (* ref Int.Map.empty *)

let param_specs = [
  ("--id", Arg.Set_int node_id, "Specify this node's id");
  ("--port", Arg.Set_int port, "Specify the port this node listens on");
  ("--peers", Arg.Set_string peer_list_string, "CSV list of peer ID:PORT mappings");
]


let main () =
  Arg.parse param_specs (fun anon -> ()) "Provide values for --id, --port, --peers";

  Printf.printf "[Node %i]: Listening on port %i" !node_id !port


let () = 
  main ()
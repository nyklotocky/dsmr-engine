open Core
open Async

type node_config = {
	id : int;
	port : int;
	peers : string;
	color : string;
}

let color_reset = "\x1b[0m"
let colors = [| "\x1b[36m"; "\x1b[35m"; "\x1b[33m" |]

let node_configs =
	[
		{ id = 1; port = 8001; peers = "2:8002,3:8003"; color = colors.(0) };
		{ id = 2; port = 8002; peers = "1:8001,3:8003"; color = colors.(1) };
		{ id = 3; port = 8003; peers = "1:8001,2:8002"; color = colors.(2) };
	]

let pipe_log prefix color reader =
	let pipe = Reader.pipe reader in
	Pipe.iter_without_pushback pipe ~f:(fun line ->
			Core.printf "%s[%s]%s %s\n%!" color prefix color_reset line)

let build_node_binary () =
	Core.printf "[Orchestrator] Pre-building raft_node executable...\n%!";
	Process.create_exn ~prog:"dune" ~args:["build"; "bin/raft_node.exe"] ()
	>>= fun proc ->
	Process.wait proc >>= function
	| Ok () ->
			Core.printf "[Orchestrator] Build successful. Launching cluster...\n%!";
			Deferred.unit
	| Error err ->
			Core.eprintf "[Orchestrator] Build failed: %s\n%!" (Core_unix.Exit_or_signal.sexp_of_error err |> Sexp.to_string_hum);
			shutdown 1;
			Deferred.unit

let spawn_node (cfg : node_config) =
	let args =
		[
			"--id";
			Int.to_string cfg.id;
			"--port";
			Int.to_string cfg.port;
			"--peers";
			cfg.peers;
		]
	in
	Process.create_exn ~prog:"./_build/default/bin/raft_node.exe" ~args () >>= fun proc ->
	let prefix = sprintf "Node %d" cfg.id in
	don't_wait_for (pipe_log prefix cfg.color (Process.stdout proc));
	don't_wait_for (pipe_log prefix cfg.color (Process.stderr proc));
	Deferred.return proc

(** Non-deprecated process signal targeting using Core_unix *)
let cleanup_nodes processes =
	Core.printf "\n[Orchestrator] Shutting down cluster nodes...\n%!";
	List.iter processes ~f:(fun proc ->
			let pid = Process.pid proc in
			match Signal_unix.send Signal.term (`Pid pid) with
			| `Ok -> ()
			| `No_such_process -> ());
	Deferred.unit

(** Non-deprecated Async signal trapping using Signal.handle *)
let wait_for_shutdown_signal () =
	let shutdown_ivar = Ivar.create () in
	let handle_signal sig_val =
		if Ivar.is_empty shutdown_ivar then Ivar.fill_exn shutdown_ivar sig_val
	in
	Signal.handle [ Signal.int; Signal.term ] ~f:handle_signal;
	Ivar.read shutdown_ivar

let run_cluster () =
	build_node_binary () >>= fun () ->
	Deferred.List.map ~how:`Sequential node_configs ~f:spawn_node >>= fun processes ->
	
	wait_for_shutdown_signal () >>= fun _sig ->
	cleanup_nodes processes >>= fun () ->
	shutdown 0;
	Deferred.unit

let command =
	Command.async
		~summary:"Spawn and orchestrate a local multi-node Raft cluster"
		(Command.Param.return run_cluster)

let () = Command_unix.run command
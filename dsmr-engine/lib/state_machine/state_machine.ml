open Core

module Command = struct
	type t =
		| Put of { key : string; value : string }
		| Delete of { key : string }
		| Compare_and_swap of { key : string; expected : string option; target : string }
end

module Response = struct
	type t =
		| Ok
		| Value of string option
		| Cas_failed of string option
		| Error of string
end

module Log_entry = struct
	type t = {
		index : int64;
		timestamp : int64;
		command : Command.t;
	}
end

module State_machine = struct
	type t = {
		storage : string String.Map.t;
		last_applied_index : int64;
	}

	let create () = {
		storage = String.Map.empty;
		last_applied_index = 0L;
	}

	let apply state_machine (log_entry : Log_entry.t) =
		let next_applied_index = Int64.(state_machine.last_applied_index + 1L) in
		if not (Int64.equal log_entry.index next_applied_index) then
			(state_machine, Response.Error "Received out of sequence index")
		else
			match log_entry.command with
			| Put { key; value }-> ({
				 storage = String.Map.set state_machine.storage ~key:key ~data:value;
				 last_applied_index = next_applied_index
				}, Response.Ok)
			| Delete { key } -> (
				let prev_value = String.Map.find state_machine.storage key in
				({
					storage = String.Map.remove state_machine.storage key;
					last_applied_index = next_applied_index
				}, Response.Value prev_value)
			)
			| Compare_and_swap { key; expected; target } ->
				let curr_value = String.Map.find state_machine.storage key in
				if Option.equal String.equal curr_value expected then
					({
						storage = String.Map.set state_machine.storage ~key:key ~data:target;
						last_applied_index = next_applied_index
					}, Response.Ok)
				else
					(state_machine, Response.Cas_failed curr_value)

	let state_hash state_machine =
		let open Digestif in
		let hash_function = fun ~key ~data ctx ->
			let key_len = Int.to_string (String.length key) in
			let value_len = Int.to_string (String.length data) in
			let hash1 = SHA256.feed_string ctx key_len in
			let hash2 = SHA256.feed_string hash1 key in
			let hash3 = SHA256.feed_string hash2 value_len in
			SHA256.feed_string hash3 data in
		let hash_context = SHA256.init () in
		let hashed_state = String.Map.fold state_machine.storage ~init:hash_context ~f:hash_function in

		SHA256.to_hex (SHA256.get hashed_state)


end
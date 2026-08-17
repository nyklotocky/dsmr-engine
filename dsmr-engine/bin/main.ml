open Dsmr_engine.State_machine

let main () =
	let log_entries = [
		{ Log_entry.index = 1L; timestamp = 1L; command = Put { key = "Account A"; value = "1000" } };
		{ Log_entry.index = 2L; timestamp = 2L; command = Put { key = "Account B"; value = "2000" } };
		{ Log_entry.index = 3L; timestamp = 3L; command = Compare_and_swap { key = "Account A"; expected = Some "1000"; target = "1500" } };
		{ Log_entry.index = 4L; timestamp = 4L; command = Delete { key = "Account B" } };
		] in

	let apply_entries state_machine = List.fold_left (fun acc le -> let sm, response = State_machine.apply acc le in sm) state_machine log_entries in
		
	let state_machine_1 = State_machine.create () in
	let final_sm_1 = apply_entries state_machine_1 in
	let hash_1 = State_machine.state_hash final_sm_1 in

	let state_machine_2 = State_machine.create () in
	let final_sm_2 = apply_entries state_machine_2 in
	let hash_2 = State_machine.state_hash final_sm_2 in

	if String.equal hash_1 hash_2 then
		Printf.printf "Hashes match! (%s)\n" hash_1

	else
		Printf.printf "Hash mismatch: %s != %s\n" hash_1 hash_2



let () = main ()

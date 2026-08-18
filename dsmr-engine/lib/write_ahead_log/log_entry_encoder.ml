open State_machine

(* 
	Payloads on disk are stored as:

	4 bytes of payload length
	-------------------------
	4 byte CRC32 Checksum
	8 byte log index
	8 byte timestamp
	1 byte command type (Put / Delete / CAS)
	N remaining bytes of command payload

			Commands are stored as:
			- Put
				4 bytes key length
				N bytes key
				4 bytes value length
				M bytes value

			- Delete
				4 bytes key length
				N bytes key

			- Compare and Swap
				4 bytes key length
				N bytes key
				1 byte has_expected flag
					when 1:
					4 bytes expected length
					M bytes expected
				4 bytes target
				K bytes target
	*)
	
	module Log_entry_encoder = struct
	let decode_command buffer command_type =
		let decode_value start =
			let value_length = Int32.to_int (Bytes.get_int32_le buffer start) in
			let value = Bytes.sub_string buffer (start + 4) value_length in
			(value_length, value) in

		match command_type with
		|1 -> let key_length, key = decode_value 21 in
			let _, value = decode_value (25 + key_length) in
			Command.Put { key; value }
			
		|2 -> let _, key = decode_value 21 in
			Delete { key }

		|3 -> let key_length, key = decode_value 21 in
			let has_expected = Bytes.get_uint8 buffer (25 + key_length) in
			let expected_length, expected = (
				
				if has_expected = 1 then
					let expected_length, expected = decode_value (26 + key_length) in
					(expected_length, Some expected)
				else
					(0, None)
					) in
					
			let _, target = decode_value (30 + key_length + expected_length) in
			
			Compare_and_swap { key; expected; target }
		|_ -> failwith "Invalid command type found in log entry"

	let encode_command (buffer : Bytes.t) (command : Command.t) : string =
		let len s = Int32.of_int (String.length s) in
		let encode_value start value =
			let val_len = len value in
			let val_len_int = Int32.to_int val_len in
			Bytes.set_int32_le buffer start val_len;
			Bytes.blit_string value 0 buffer (start + 4) val_len_int;
			val_len_int in

		let cmd_start = 25 in
		
		(
		match command with
		| Put p -> 
			let key_len = encode_value cmd_start p.key in
			let _ = encode_value (cmd_start + 4 + key_len) p.value in ()

		| Delete d -> let _ = encode_value cmd_start d.key in ()

		| Compare_and_swap cas -> match cas.expected with 
			| Some ex ->
				let key_len = encode_value cmd_start cas.key in
				let ex_len = encode_value (cmd_start + 5 + key_len) ex in
				let _ = encode_value (cmd_start + 9 + key_len + ex_len) cas.target in
				Bytes.set_uint8 buffer (cmd_start + 4 + key_len) 1 (* set flag to true *)

			| None ->
				let key_len = encode_value cmd_start cas.key in
				let _ = encode_value (cmd_start + 5 + key_len) cas.target in
				Bytes.set_uint8 buffer (cmd_start + 4 + key_len) 0;
		);

		Bytes.sub_string buffer cmd_start ((Bytes.length buffer) - cmd_start)

	let compute_checksum li ts ct pl =
		let payload_string = Printf.sprintf "%Li %Li %i %s" li ts ct pl in

		Crc.Crc32.string payload_string 0 (String.length payload_string)
		
	let encode (log_entry : Log_entry.t) : Bytes.t =
		let command_size = match log_entry.command with
		| Put p -> 8 + String.length p.key + String.length p.value
		| Delete d -> 4 + String.length d.key
		| Compare_and_swap cas -> 9 + String.length cas.key + String.length cas.target + (
			match cas.expected with
			| None -> 0
			| Some ex -> 4 + String.length ex
		)
		in

		let buffer = Bytes.create (25 + command_size) in
		
		(* Write size of payload *)
		Bytes.set_int32_le buffer 0 (Int32.of_int (21 + command_size));
		(* Write checksum *)
		let command_type = match log_entry.command with
			| Put _ -> 1
			| Delete _ -> 2
			| Compare_and_swap _ -> 3 in
		let payload_string = encode_command buffer log_entry.command in
		let checksum = compute_checksum log_entry.index log_entry.timestamp command_type payload_string in

		Bytes.set_int32_le buffer 4 checksum;
		Bytes.set_int64_le buffer 8 log_entry.index;
		Bytes.set_int64_le buffer 16 log_entry.timestamp;
		Bytes.set_uint8 buffer 24 command_type;
		buffer

	let decode (buffer : Bytes.t) : Log_entry.t option =
		let checksum = Bytes.get_int32_le buffer 0 in
		let log_index = Bytes.get_int64_le buffer 4 in
		let timestamp = Bytes.get_int64_le buffer 12 in
		let command_type = Bytes.get_uint8 buffer 20 in
		let payload_as_string = Bytes.sub_string buffer 21 ((Bytes.length buffer) - 21) in
		let computed_checksum = compute_checksum log_index timestamp command_type payload_as_string in
		
		if checksum = computed_checksum then
			Some {
				index = log_index;
				timestamp;
				command = decode_command buffer command_type;
			}
		else
			None
		
		
end
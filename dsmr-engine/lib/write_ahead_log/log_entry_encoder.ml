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

			let _, target = decode_value (26 + key_length + expected_length) in
			
			Compare_and_swap { key; expected; target }
		|_ -> failwith "Invalid command type found in log entry"

	let compute_checksum li ts ct pl =
		let payload_string = Printf.sprintf "%Li %Li %i %s" li ts ct pl in

		Crc.Crc32.string payload_string 0 (String.length payload_string)
		
	let encode (log_entry : Log_entry.t) : Bytes.t =
		Bytes.create 0

	let decode (buffer : Bytes.t) : Log_entry.t option =
		let checksum = Bytes.get_int32_le buffer 0 in
		let log_index = Bytes.get_int64_le buffer 4 in
		let timestamp = Bytes.get_int64_le buffer 12 in
		let command_type = Bytes.get_uint8 buffer 20 in
		let payload = Bytes.sub_string buffer 21 ((Bytes.length buffer) - 21) in
		let computed_checksum = compute_checksum log_index timestamp command_type payload in

		if checksum = computed_checksum then
			Some {
				index = log_index;
				timestamp;
				command =	decode_command buffer command_type
			}
		else
			None
		
		
end
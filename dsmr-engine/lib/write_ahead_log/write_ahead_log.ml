open State_machine
open Log_entry_encoder

module Write_ahead_log = struct
  type t = {
    input_channel : In_channel.t
  }


  let initialize filename =
    {
      input_channel = In_channel.open_bin filename
    }


  let load_entries wal : Log_entry.t Seq.t =
    let entry_size_buf = Bytes.create 4 in
    let rec load_entry = (fun () ->
      try
        really_input wal.input_channel entry_size_buf 0 4;
        let entry_size = Int32.to_int (Bytes.get_int32_be entry_size_buf 0) in
        let entry_buf = Bytes.create entry_size in
        really_input wal.input_channel entry_buf 0 entry_size;
        let entry = Log_entry_encoder.decode entry_buf in
        match entry with
        | Some e -> Seq.cons e load_entry ()
        | None -> Seq.Nil
      with
      | End_of_file -> Seq.Nil
    ) in
    load_entry
end
open Dsmr_engine.State_machine
open Dsmr_engine.Log_entry_encoder

let log_entry_testable =
  Alcotest.testable
    (fun ppf (entry : Log_entry.t) -> Core.Sexp.pp ppf (Log_entry.sexp_of_t entry))
    (fun a b ->
      Int64.equal a.index b.index
      && Int64.equal a.timestamp b.timestamp
      && Command.compare a.command b.command = 0)

(** Helper constructor for clean test setups *)
let make_entry index timestamp command =
  { Log_entry.index; timestamp; command }

(** 1. Roundtrip Serialization Tests *)

let test_roundtrip_put () =
  let entry =
    make_entry 1L 1000L (Command.Put { key = "account_a"; value = "100" })
  in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable) "Put roundtrip matches" entry decoded
  | None -> Alcotest.fail "Failed to decode valid Put entry (checksum mismatch)"

let test_roundtrip_delete () =
  let entry = make_entry 2L 1001L (Command.Delete { key = "account_b" }) in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable) "Delete roundtrip matches" entry decoded
  | None -> Alcotest.fail "Failed to decode valid Delete entry"

let test_roundtrip_cas_some () =
  let entry =
    make_entry 3L 1002L
      (Command.Compare_and_swap
         { key = "balance"; expected = Some "100"; target = "80" })
  in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable)
        "CAS (Some expected) roundtrip matches" entry decoded
  | None -> Alcotest.fail "Failed to decode valid CAS (Some) entry"

let test_roundtrip_cas_none () =
  let entry =
    make_entry 4L 1003L
      (Command.Compare_and_swap
         { key = "new_user"; expected = None; target = "active" })
  in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable)
        "CAS (None expected) roundtrip matches" entry decoded
  | None -> Alcotest.fail "Failed to decode valid CAS (None) entry"

(** 2. Edge Cases & Boundary Conditions *)

let test_empty_keys_and_values () =
  let entry =
    make_entry 5L 1004L (Command.Put { key = ""; value = "" })
  in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable) "Empty key/value roundtrip" entry decoded
  | None -> Alcotest.fail "Failed to decode empty key/value entry"

let test_binary_data_payload () =
  (* Keys and values containing null bytes and non-ASCII binary data *)
  let binary_key = "key\x00\xff\x00" in
  let binary_val = "\x01\x02\x03\x00\x00\x0a" in
  let entry =
    make_entry 6L 1005L (Command.Put { key = binary_key; value = binary_val })
  in
  let encoded = Log_entry_encoder.encode entry in
  match Log_entry_encoder.decode encoded with
  | Some decoded ->
      Alcotest.(check log_entry_testable) "Binary payload roundtrip" entry decoded
  | None -> Alcotest.fail "Failed to decode binary payload entry"

(** 3. Corruption & Validation Tests *)

let test_corrupted_checksum () =
  let entry =
    make_entry 7L 1006L (Command.Put { key = "key"; value = "val" })
  in
  let encoded = Log_entry_encoder.encode entry in
  

  (* Intentionally corrupt 1 byte in the payload area *)
  Bytes.set_uint8 encoded 26 (Bytes.get_uint8 encoded 26 lxor 0xFF);

  match Log_entry_encoder.decode encoded with
  | None -> Alcotest.(check bool) "Checksum failure detected" true true
  | Some _ -> Alcotest.fail "Decoder accepted corrupted byte buffer!"

(** 4. Alcotest Runner *)

let () =
  let open Alcotest in
  run "Log Entry Encoder/Decoder Suite"
    [
      ( "Roundtrip Serialization",
        [
          test_case "Put command" `Quick test_roundtrip_put;
          test_case "Delete command" `Quick test_roundtrip_delete;
          test_case "Compare-and-swap (Some)" `Quick test_roundtrip_cas_some;
          test_case "Compare-and-swap (None)" `Quick test_roundtrip_cas_none;
        ] );
      ( "Edge Cases",
        [
          test_case "Empty strings" `Quick test_empty_keys_and_values;
          test_case "Binary payloads" `Quick test_binary_data_payload;
        ] );
      ( "Corruption Handling",
        [
          test_case "Corrupted checksum returns None" `Quick test_corrupted_checksum;
        ] );
    ]
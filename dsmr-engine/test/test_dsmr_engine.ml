open Core
open Dsmr_engine.State_machine (* Replace with your actual module path *)

(** 1. Custom Alcotest Testable Definitions *)

let response_testable =
  Alcotest.testable
    (fun ppf res -> Sexp.pp ppf (Response.sexp_of_t res))
    Response.equal

let string_testable = Alcotest.string

(** 2. Test Helper Functions *)

let make_entry index command =
  { Log_entry.index; timestamp = 1000L; command }

let apply_ok sm entry =
  let next_sm, response = State_machine.apply sm entry in
  (next_sm, response)

(** 3. Test Cases *)

let test_basic_mutations () =
  let sm = State_machine.create () in

  (* Put "x" = "10" *)
  let entry1 = make_entry 1L (Command.Put { key = "x"; value = "10" }) in
  let sm, res1 = apply_ok sm entry1 in
  Alcotest.(check response_testable) "Put succeeds" Response.Ok res1;

  (* Put "y" = "20" *)
  let entry2 = make_entry 2L (Command.Put { key = "y"; value = "20" }) in
  let sm, res2 = apply_ok sm entry2 in
  Alcotest.(check response_testable) "Put succeeds" Response.Ok res2;

  (* Delete "x" *)
  let entry3 = make_entry 3L (Command.Delete { key = "x" }) in
  let sm, res3 = apply_ok sm entry3 in
  Alcotest.(check response_testable)
    "Delete returns previous value"
    (Response.Value (Some "10"))
    res3;

  (* Delete non-existent key "z" *)
  let entry4 = make_entry 4L (Command.Delete { key = "z" }) in
  let _sm, res4 = apply_ok sm entry4 in
  Alcotest.(check response_testable)
    "Delete non-existent returns None"
    (Response.Value None)
    res4

let test_cas_logic () =
  let sm = State_machine.create () in

  (* Initial Put *)
  let entry1 = make_entry 1L (Command.Put { key = "bal"; value = "100" }) in
  let sm, _ = apply_ok sm entry1 in

  (* Successful CAS *)
  let entry2 =
    make_entry 2L
      (Command.Compare_and_swap
         { key = "bal"; expected = Some "100"; target = "80" })
  in
  let sm, res2 = apply_ok sm entry2 in
  Alcotest.(check response_testable) "CAS succeeds" Response.Ok res2;

  (* Failed CAS due to stale expectation *)
  let entry3 =
    make_entry 3L
      (Command.Compare_and_swap
         { key = "bal"; expected = Some "100"; target = "50" })
  in
  let sm, res3 = apply_ok sm entry3 in
  Alcotest.(check response_testable)
    "CAS fails on stale value"
    (Response.Cas_failed (Some "80") )
    res3;

  (* CAS on missing key expecting None *)
  let entry4 =
    make_entry 4L
      (Command.Compare_and_swap
         { key = "new_key"; expected = None; target = "created" })
  in
  let _sm, res4 = apply_ok sm entry4 in
  Alcotest.(check response_testable) "CAS succeeds for None expectation" Response.Ok res4

let test_index_validation () =
  let sm = State_machine.create () in

  (* Skip index 1L and go straight to 2L *)
  let invalid_entry = make_entry 2L (Command.Put { key = "a"; value = "1" }) in
  let sm_after, res = apply_ok sm invalid_entry in

  match res with
  | Response.Error msg ->
      Alcotest.(check bool)
        "Error message mentions out of order index"
        true
        (String.is_substring msg ~substring:"out of sequence index");
      (* Verify state was not modified *)
      Alcotest.(check string_testable)
        "State hash unchanged"
        (State_machine.state_hash sm)
        (State_machine.state_hash sm_after)
  | _ -> Alcotest.fail "Expected Out of Order Error response"

let test_hash_determinism () =
  (* Test 1: Insertion order independence *)
  let sm_a = State_machine.create () in
  let sm_a, _ = apply_ok sm_a (make_entry 1L (Command.Put { key = "k1"; value = "v1" })) in
  let sm_a, _ = apply_ok sm_a (make_entry 2L (Command.Put { key = "k2"; value = "v2" })) in

  let sm_b = State_machine.create () in
  let sm_b, _ = apply_ok sm_b (make_entry 1L (Command.Put { key = "k2"; value = "v2" })) in
  let sm_b, _ = apply_ok sm_b (make_entry 2L (Command.Put { key = "k1"; value = "v1" })) in

  Alcotest.(check string_testable)
    "State hashes match regardless of entry sequence ordering"
    (State_machine.state_hash sm_a)
    (State_machine.state_hash sm_b);

  (* Test 2: Full log replay equality *)
  let log_stream = [
    make_entry 1L (Command.Put { key = "a"; value = "10" });
    make_entry 2L (Command.Put { key = "b"; value = "20" });
    make_entry 3L (Command.Compare_and_swap { key = "a"; expected = Some "10"; target = "15" });
    make_entry 4L (Command.Delete { key = "b" });
  ] in

  let run_stream stream =
    List.fold stream ~init:(State_machine.create ()) ~f:(fun sm entry ->
        let next_sm, _ = State_machine.apply sm entry in
        next_sm)
  in

  let engine_1 = run_stream log_stream in
  let engine_2 = run_stream log_stream in

  Alcotest.(check string_testable)
    "Replayed engines produce bit-identical state hashes"
    (State_machine.state_hash engine_1)
    (State_machine.state_hash engine_2)

(** 4. Test Runner Suite Registry *)

let () =
  let open Alcotest in
  run "Deterministic State Machine"
    [
      ( "Mutations",
        [
          test_case "Basic operations (Put, Delete)" `Quick test_basic_mutations;
          test_case "Compare-and-swap semantics" `Quick test_cas_logic;
        ] );
      ( "Invariants",
        [
          test_case "Out-of-order index rejection" `Quick test_index_validation;
        ] );
      ( "Determinism",
        [
          test_case "Order independence & replay hash equality" `Quick test_hash_determinism;
        ] );
    ]
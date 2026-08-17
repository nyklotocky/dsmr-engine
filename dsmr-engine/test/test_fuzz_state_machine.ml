open Core
open Dsmr_engine.State_machine

(** 1. Custom Generators for Commands and Log Entries *)

let key_gen =
  QCheck.Gen.(
    map (fun s -> "key_" ^ s) (string_size ~gen:(char_range 'a' 'z') (int_range 1 4)))

let value_gen =
  QCheck.Gen.(
    map (fun i -> string_of_int i) (int_range 1 1000))

let command_gen =
  let open QCheck.Gen in
  oneof [
    map2 (fun key value -> Command.Put { key; value }) key_gen value_gen;
    map (fun key -> Command.Delete { key }) key_gen;
    map3
      (fun key expected_opt target ->
        Command.Compare_and_swap { key; expected = expected_opt; target })
      key_gen
      (option value_gen)
      value_gen;
  ]

(** Generates a valid, monotonically sequential stream of Log_entry records *)
let log_stream_gen =
  let open QCheck.Gen in
  let* commands = list_size (int_range 10 200) command_gen in
  let entries =
    List.mapi commands ~f:(fun idx cmd ->
        {
          Log_entry.index = Int64.of_int (idx + 1);
          timestamp = Int64.of_int (1000 + idx);
          command = cmd;
        })
  in
  return entries

let log_stream_arbitrary =
  QCheck.make
    ~print:(fun entries ->
      Printf.sprintf "[Log Stream: %d entries]" (List.length entries))
    log_stream_gen

(** 2. Property Definitions *)

(** Property 1: Replay Determinism
    Running a randomized log stream on two clean engines produces identical state hashes. *)
let prop_replay_determinism =
  QCheck.Test.make
    ~count:500 (* Run 500 randomized fuzz trials *)
    ~name:"Replay determinism: Engine A and Engine B yield bit-identical hashes"
    log_stream_arbitrary
    (fun log_stream ->
      let run_engine entries =
        List.fold entries ~init:(State_machine.create ()) ~f:(fun sm entry ->
            let next_sm, _ = State_machine.apply sm entry in
            next_sm)
      in
      let engine_a = run_engine log_stream in
      let engine_b = run_engine log_stream in
      let hash_a = State_machine.state_hash engine_a in
      let hash_b = State_machine.state_hash engine_b in
      String.equal hash_a hash_b)

(** Property 2: Monotonic Index Progression & Safe Transitions
    Applying N entries advances last_applied_index to exactly N without crashing. *)
let prop_monotonic_index =
  QCheck.Test.make
    ~count:500
    ~name:"Monotonic index: last_applied_index equals total processed entries"
    log_stream_arbitrary
    (fun log_stream ->
      let final_sm =
        List.fold log_stream ~init:(State_machine.create ()) ~f:(fun sm entry ->
            let next_sm, _ = State_machine.apply sm entry in
            next_sm)
      in
      let expected_count = Int64.of_int (List.length log_stream) in
      Int64.equal final_sm.last_applied_index expected_count)

(** 3. Integration with Alcotest Runner *)

let () =
  let suite =
    List.map
      [ prop_replay_determinism; prop_monotonic_index ]
      ~f:QCheck_alcotest.to_alcotest
  in
  Alcotest.run "Determinism QCheck Fuzzing Suite" [ ("Fuzz Properties", suite) ]
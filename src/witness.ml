let write_witness (bug_type : Formula.bug_type) (pos : Filepath.position) =
  let time = Unix.time () |> Unix.gmtime in
  let time =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (time.tm_year + 1900) (* years since 1900 *)
      (time.tm_mon + 1) (* months 0..11 *)
      time.tm_mday time.tm_hour time.tm_min time.tm_sec
  in

  let data_model =
    match (Machine.get_machdep ()).machdep_name with
    | "machdep_x86_32" -> "ILP32"
    | "machdep_x86_64" -> "LP64"
    | other -> Common.fail "unknown machdep: %s" other
  in

  let filename, filepath =
    File.get_all () |> function
    | (File.NeedCPP (path, _, _, _) | File.NoCPP path | External (path, _)) :: _
      ->
        (Filepath.basename path, Filepath.to_string_abs path)
    | _ -> Common.fail "could not find filepath"
  in

  let file = open_in filepath in
  let hash = Sha256.input file |> Sha256.to_hex in
  close_in file;

  let specification =
    match bug_type with
    | Formula.Invalid_memtrack _ ->
        "CHECK( init(main()), LTL(G valid-memtrack) )"
    | Formula.Invalid_deref _ -> "CHECK( init(main()), LTL(G valid-deref) )"
    | Formula.Invalid_free _ -> "CHECK( init(main()), LTL(G valid-free) )"
  in

  let uuid = Uuidm.v4_gen (Random.get_state ()) () |> Uuidm.to_string in

  let oc = open_out "witness.yml" in
  Printf.fprintf oc
    "- content:\n\
    \  - segment: \n\
    \    - waypoint:\n\
    \        type: target\n\
    \        action: 'follow'\n\
    \        location:\n\
    \          file_name: '%s'\n\
    \          line: %i\n\
    \  entry_type: violation_sequence\n\
    \  metadata:\n\
    \    creation_time: '%s'\n\
    \    format_version: '2.0'\n\
    \    producer:\n\
    \      name: KTSN\n\
    \      version: '0.1'\n\
    \    task:\n\
    \      data_model: %s\n\
    \      input_file_hashes:\n\
    \        %s: %s\n\
    \      input_files:\n\
    \      - %s\n\
    \      language: C\n\
    \      specification: %s\n\
    \    uuid: %s\n"
    filename pos.pos_lnum time data_model filepath hash filepath specification
    uuid;
  close_out oc

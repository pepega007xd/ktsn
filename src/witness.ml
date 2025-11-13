let write_witness (bug_type : Formula.bug_type) (pos : Filepath.position) =
  let time = Unix.time () |> Unix.gmtime in
  let time =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (time.tm_year + 1900) (* years since 1900 *)
      (time.tm_mon + 1) (* months 0..11 *)
      time.tm_mday time.tm_hour time.tm_min time.tm_sec
  in

  let data_model =
    match Machine.sizeof_ptr () with
    | 4 -> "ILP32"
    | 8 -> "LP64"
    | _ -> Common.fail "unsupported machdep for sv-witnesses: %s" (Machine.machdep_name ())
  in

  let architecture =
    match Machine.sizeof_ptr () with
    | 4 -> "32bit"
    | 8 -> "64bit"
    | _ -> Common.fail "unsupported machdep for sv-witnesses: %s" (Machine.machdep_name ())
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
    \      name: SEAL\n\
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
  close_out oc;
  let oc = open_out "witness.graphml" in
  Printf.fprintf oc
    "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n\
     <graphml xmlns='http://graphml.graphdrawing.org/xmlns'\n\
    \  xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>\n\
    \ <key attr.name='isViolationNode'\n\
    \    attr.type='boolean' for='node' id='violation'/>\n\
    \ <key attr.name='isEntryNode' attr.type='boolean' for='node' id='entry'>\n\
    \    <default>false</default>\n\
    \ </key>\n\
    \ <key attr.name='sourcecodeLanguage'\n\
    \    attr.type='string' for='graph' id='sourcecodelang'/>\n\
    \ <key attr.name='programFile'\n\
    \    attr.type='string' for='graph' id='programfile'/>\n\
    \ <key attr.name='programHash'\n\
    \    attr.type='string' for='graph' id='programhash'/>\n\
    \ <key attr.name='specification'\n\
    \    attr.type='string' for='graph' id='specification'/>\n\
    \ <key attr.name='architecture'\n\
    \    attr.type='string' for='graph' id='architecture'/>\n\
    \ <key attr.name='producer'\n\
    \    attr.type='string' for='graph' id='producer'/>\n\
    \ <key attr.name='creationTime'\n\
    \    attr.type='string' for='graph' id='creationtime'/>\n\
    \ <key attr.name='witness-type'\n\
    \    attr.type='string' for='graph' id='witness-type'/>\n\
    \ <key attr.name='inputWitnessHash'\n\
    \    attr.type='string' for='graph' id='inputwitnesshash'/>\n\
    \ <graph edgedefault='directed'>\n\
    \  <data key='witness-type'>violation_witness</data>\n\
    \  <data key='sourcecodelang'>C</data>\n\
    \  <data key='producer'>SEAL</data>\n\
    \  <data key='specification'>%s</data>\n\
    \  <data key='programfile'>%s</data>\n\
    \  <data key='programhash'>%s</data>\n\
    \  <data key='architecture'>%s</data>\n\
    \  <data key='creationtime'>%s</data>\n\
    \  <node id='entry'>\n\
    \   <data key='entry'>true</data>\n\
    \  </node>\n\
    \  <node id='violation'>\n\
    \   <data key='violation'>true</data>\n\
    \  </node>\n\
    \  <edge source='entry' target='violation' />\n\
    \ </graph>\n\
     </graphml>\n"
    specification filepath hash architecture time;
  close_out oc

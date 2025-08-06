let capture_analysis_output_to_string f =
  let tmp_output_file = Filename.temp_file "mopsa_output_" ".txt" in
  let oc = open_out tmp_output_file in
  let fd = Unix.descr_of_out_channel oc in

  let original_stdout = Unix.dup Unix.stdout in
  Unix.dup2 fd Unix.stdout;

  let result =
    try f ()
    with e ->
      Unix.dup2 original_stdout Unix.stdout;
      close_out oc;
      raise e
  in

  Unix.dup2 original_stdout Unix.stdout;
  close_out oc;

  let ic = open_in tmp_output_file in
  let output = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Unix.close original_stdout;

  (output, result, tmp_output_file)

let run_analysis code_text =
  (* create .py file *)
  let tmp_filename = Filename.temp_file "mopsa_input_" ".py" in
  let channel = open_out tmp_filename in
  output_string channel code_text;
  close_out channel;

  let argv =
    [|
      "./_build/default/bin/main.exe";
      "-config";
      "./values.json";
      "-share-dir=share/mopsa";
      tmp_filename;
    |]
  in

  let captured_output, result_code, tmp_output_file =
    (* let run () = exit @@ parse_options Sys.argv analyze_files () *)
    capture_analysis_output_to_string (fun () ->
        Mopsa_analyzer.Framework.Runner.parse_options argv
          Mopsa_analyzer.Framework.Runner.analyze_files ())
  in
  Printf.printf "Captured (exit %d): %s\n" result_code captured_output;

  (* Delte tmp files *)
  Printf.printf "Tmp file name: %s & %s\n" tmp_filename tmp_output_file;
  Unix.unlink tmp_filename;
  Unix.unlink tmp_output_file;

  (* return *)
  (result_code, captured_output)

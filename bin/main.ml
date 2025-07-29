let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ ->
      let code = Utils.code in
      let code_simple = Utils.code_simple in
      let error = Utils.error in
      let exitcode, output = Analysis.run_analysis code in
      let mopsa = if exitcode = 1 then output else "" in
      let%lwt llm_output = Llm.response code_simple error mopsa in
      Dream.html llm_output)
  ]
  (* let code = Utils.code in
  let code_simple = Utils.code_simple in
  let error = Utils.error in
  let t1 = Unix.gettimeofday () in
  let exitcode, output = Analysis.run_analysis code in
  let t2 = Unix.gettimeofday () in
  let mopsa = if exitcode = 1 then output else "" in
  let llm_output = Llm.response code_simple error mopsa in
  let t3 = Unix.gettimeofday () in
  Printf.printf "===========================================================\n
  Model answer: \n %s" llm_output;
  Printf.printf "Mopsa anslysis time: %.6f seconds\n LLM analysis time: %.6f" (t2 -. t1) (t3 -. t2) *)




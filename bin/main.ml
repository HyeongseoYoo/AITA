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
      let output = Utils.extract llm_output in
      Dream.html output)
  ]

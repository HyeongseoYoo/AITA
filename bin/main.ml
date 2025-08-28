let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             let code = Utils.code in
             let code_simple = Utils.code_simple in
             let code_full = Utils.code_full in
             let error = Utils.error in
             let exitcode, output = Analysis.run_analysis code in
             let mopsa = if exitcode = 1 then output else "" in
             let%lwt llm_output = Llm.response code_simple code_full error mopsa in
             (* let output = Utils.extract llm_output in *)
             Dream.html llm_output);
       ]

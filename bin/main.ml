let () =
  let code = Utils.code in
  let error = Utils.error in
  let exitcode, output = Analysis.run_analysis code in
  let mopsa = if exitcode = 1 then output else "" in
  let llm_output = Llm.response code error mopsa in
  Printf.printf "===========================================================\n
  Model answer: \n %s" llm_output 



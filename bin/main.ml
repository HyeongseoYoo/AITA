open Lwt.Syntax

let cors_filter hander request =
  match Dream.method_ request with
  | `OPTIONS -> 
      Dream.respond ~headers:[
        ("Access-Control-Allow-Origin", "*");
        ("Access-Control-Allow-Headers", "Content-Type");
        ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
      ] ~status:`No_Content ""
  | _ ->
      let%lwt response = hander request in
      Dream.add_header response "Access-Control-Allow-Origin" "*";
      Dream.add_header response "Access-Control-Allow-Headers" "Content-Type";
      Lwt.return response


let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ cors_filter
  @@ Dream.router
       [
         Dream.options "/login" (fun _ -> Dream.empty `OK);
         Dream.post "/login" (fun _request ->
             let session_id = Dream.random 32 |> Dream.to_base64url in
             let () = Hashtbl.add Utils.user_chat_table session_id 0 in
             let res : Dto.login_response_body = { session_id } in
             let body =
               res |> Dto.login_response_body_to_yojson |> Yojson.Safe.to_string
             in
             Dream.json body);
         Dream.options "/analysus" (fun _ -> Dream.empty `OK);
         Dream.post "/analysis" (fun request ->
             let* body = Dream.body request in
             let req = body |> Yojson.Safe.from_string |> Dto.analysis_request_body_of_yojson in
             match req with 
             | Ok req ->
                let code = req.code in
                let code_full = 
                  let context_codes = List.map (fun (c : Dto.context_cell) -> c.code) req.context in
                  String.concat "\n" (context_codes @ [code])
                in
                let mopsa_code = 
                  let context_codes_wrapped =
                    List.map (fun (c : Dto.context_cell) -> Utils.add_try c.code) req.context in
                  String.concat "\n" (context_codes_wrapped @ [code; "\n"])
                in
                let exitcode, output = Analysis.run_analysis mopsa_code in
                let mopsa = if exitcode = 1 then output else "" in
                let stderr, hint, stdout = 
                  match req.output with
                  | Some o -> 
                      Utils.get_error_and_hints o
                  | None -> "", "", ""
                in 
                Dream.log "\n[LOG] ==== code ====\n%s\n[LOG] ==== mopsa ====\n%s\n" code mopsa;
                Dream.log "\n[LOG] stderr: %s\n[LOG] stdout: %s\n[LOG] ==== hint ====\n%s\n" stderr stdout hint;
                Dream.stream ~headers:[("Content-Type", "application/x-ndjson; charset=utf-8")] (fun s ->
                  let chat_id = Utils.chat_id_of req.session_id in
                  let _ = Hashtbl.add Utils.chat_parser_table chat_id (Utils.create_streaming_parser ()) in
                  let%lwt () =
                    Dream.write s ({|{"chat_id":"|} ^ chat_id ^ {|"}|} ^ "\n")
                  in
                  let on_chunk (txt : string) =
                    let json_str = Utils.response_to_json txt chat_id in
                    let* () = Dream.write s json_str in
                    Dream.flush s
                  in
                  let on_error (msg : string) =
                    Dream.write s
                      (Yojson.Basic.to_string (`Assoc [("code", `String "INTERNAL SERVER ERROR"); ("message", `String msg)]))
                  in
                  Llm.stream_response ~on_chunk ~on_error code code_full stderr mopsa hint stdout
                )
             | Error err -> Dream.json ~status:`Bad_Request err);
         Dream.post "/chat" (fun request ->
             let* body = Dream.body request in
             let req = body |> Yojson.Safe.from_string |> Dto.chat_request_body_of_yojson in
             match req with
             | Ok _req ->
              let response : Dto.chat_response_body = {
                explanation = "dictionary key, value 값 중 어떤 것을 사용해야 하는지 모호할 때, TypeError가 발생 할 수 있습니다.";
                follow_ups = ["비슷한 오류가 발생하는 다른 프로그램을 보여주세요"; "이해됐어요ㅠㅠ!"]}
              in
              response |> Dto.chat_response_body_to_yojson |> Yojson.Safe.to_string |> Dream.json
             | Error err -> Dream.json ~status:`Bad_Request err);
       ]

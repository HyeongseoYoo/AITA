open Lwt.Infix

(* !! should change !! *)
let api_key = "PUT YOUR KEY"

let model = "claude-sonnet-4-20250514"

let endpoint =
  Uri.of_string "https://api.anthropic.com/v1/messages"

let headers =
  Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("x-api-key", api_key);
    ("anthropic-version", "2023-06-01")
  ]

let make_request_json user_input =
  `Assoc [
    ("model", `String model);
    ("max_tokens", `Int 1024);
    ("messages", `List [
      `Assoc [
        ("role", `String "user");
        ("content", `String user_input)
      ]
    ])
  ]

let prompt ?(code="") ?(error="") ?(mopsa="") ()=
  let normalize s = if String.trim s = "" then "없음" else s in
  let code = normalize code in
  let error = normalize error in
  let mopsa = normalize mopsa in
  "다음은 프로그램 실행 중 발생한 에러 메시지와 MOPSA 정적분석 결과야.\n\
   코드와 분석 결과를 줄테니 이 에러가 발생한 이유와 고치는 방법을 알려줘.
   Python 기초 수업을 듣는 학생이 짠 프로그램이니 쉽고 간단하게 알려줘.:\n\n"
  ^ "Code: " ^ code
  ^ "위 코드에서 Try ~ exception"
  ^ "Python Error: " ^ error
  ^ "mopsa 정적분석 결과: " ^ mopsa

let call_claude user_input =
  let body_json = make_request_json user_input |> Yojson.Safe.to_string in
  Cohttp_lwt_unix.Client.post
    ~headers
    ~body:(Cohttp_lwt.Body.of_string body_json)
    endpoint
  >>= fun (resp, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream >|= fun body_str ->
  match resp.Cohttp.Response.status with
  | `OK -> body_str
  | status ->
    Printf.sprintf "Error: %s\nResponse: %s"
      (Cohttp.Code.string_of_status status)
      body_str

let response code error mopsa = 
  let message = prompt ~code:code ~error:error ~mopsa:mopsa () in
  Lwt_main.run (call_claude message) 
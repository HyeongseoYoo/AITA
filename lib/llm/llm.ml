open Lwt.Infix

(* !!!! *)
let api_key = ""
let model = "claude-sonnet-4-20250514"
let endpoint = Uri.of_string "https://api.anthropic.com/v1/messages"

let headers =
  Cohttp.Header.of_list
    [
      ("Content-Type", "application/json");
      ("x-api-key", api_key);
      ("anthropic-version", "2023-06-01");
    ]

let make_request_json user_input =
  `Assoc
    [
      ("model", `String model);
      ("max_tokens", `Int 1024);
      ( "messages",
        `List
          [
            `Assoc [ ("role", `String "user"); ("content", `String user_input) ];
          ] );
    ]

let prompt ?(code = "") ?(error = "") ?(mopsa = "") () =
  let normalize s = if String.trim s = "" then "없음" else s in
  let code = normalize code in
  let error = normalize error in
  let mopsa = normalize mopsa in
  "Python 기초 수업을 듣는 학생이 colab에서 짠 코드와 그 블록 실행 시 발생하는 에러야.\n\n\
  \  이거에 추가적으로 전체코드를 MOPSA 정적분석기로 돌려서 나온 결과까지 함께 알려줄테니까 어디를 고쳐야되는지 알려줘.\n\n\
  \  MOPSA 에러에 있는 코드 line number와 실제 내가 전달하는 Code의 line number는 다를 수 있으니 유의해."
  ^ "Code: " ^ code ^ "Python Error: " ^ error ^ "mopsa 정적분석 결과: " ^ mopsa
  ^ "TypeError에서 이와 같은 실수는 i in dict에서 꺼내는 값이 value 값이 아닌 key 값이라는 것 인지하지 못하기 \
     때문에 발생해. 학생은 value가 나올줄 알고 비교를 한거지. 하지만 key가 나온다는 점을 알려줘야해."

let call_claude user_input =
  let body_json = make_request_json user_input |> Yojson.Safe.to_string in
  Cohttp_lwt_unix.Client.post ~headers
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
  let message = prompt ~code ~error ~mopsa () in
  call_claude message
(* Lwt_main.run (call_claude message) *)

open Lwt.Syntax

let api_key =
  match Sys.getenv "OPENROUTER_API_KEY" |> String.trim with
  | k when k <> "" -> k
  | _ | (exception Not_found) -> failwith "OPENROUTER_API_KEY is not set"

let model =
  match Sys.getenv "OPENROUTER_MODEL" |> String.trim with
  | m when m <> "" -> m
  | _ | (exception Not_found) -> "@preset/aita"

let endpoint = Uri.of_string "https://openrouter.ai/api/v1/chat/completions"

let headers =
  Cohttp.Header.of_list
    [
      ("Content-Type", "application/json");
      ("Authorization", "Bearer " ^ api_key);
    ]

type message = { role : string; content : string } [@@deriving yojson]
type reasoning = { exclude : bool } [@@deriving yojson]

type response_format = {
  typ : string; [@key "type"]
  json_schema : Yojson.Safe.t;
}
[@@deriving yojson]

type request = {
  model : string;
  max_tokens : int;
  stream : bool;
  reasoning : reasoning;
  messages : message list;
  response_format : response_format;
}
[@@deriving yojson]

let json_schema =
  Yojson.Safe.from_string
    {|{
  "name": "code_fix_feedback",
  "strict": true,
  "schema": {
    "type": "object",
    "properties": {
      "explanation": {"type": "string", "description": "학생에게 보여줄 코드 분석 및 원인 설명"},
      "followUps": {"type": "array", "items": {"type": "string"}, "description": "학생에게 제공할 후속 질문 목록"}
    },
    "required": ["explanation", "followUps"],
    "additionalProperties": false
  }
}|}

let make_request_json system_input user_input =
  request_to_yojson
    {
      model;
      max_tokens = 10000;
      reasoning = { exclude = true };
      stream = true;
      messages = [ { role = "system"; content = system_input } ; { role = "user"; content = user_input } ];
      response_format = { typ = "json_schema"; json_schema };
    }

let system_input =
  "당신은 Python 기초 수업을 듣는 학생들을 도와주는 친절한 튜터입니다.\n\n"
  ^ "역할:\n"
  ^ "- 학생의 코드에서 발생한 문제를 분석하고 설명합니다\n"
  ^ "- 문제의 원인을 이해하기 쉽게 설명합니다\n"
  ^ "- 학생이 추가로 궁금해할 만한 질문들을 제안합니다\n\n"
  ^ "응답 규칙:\n"
  ^ "- 인사말은 생략하고 바로 문제 분석부터 시작하세요\n"
  ^ "- 한국어로 친절하고 상세하게 설명하세요\n"
  ^ "- followUps에는 질문만 간략하게 작성하고, 답변은 미리 제공하지 마세요\n"
  ^ "- 주어진 JSON 형식에 맞게 응답하세요\n\n"


let prompt_user ?(code = "") ?(code_full = "") ?(stderr = "") ?(stdout = "") ?(mopsa = "") ?(hint = "") () =
  let normalize s = if String.trim s = "" then "없음" else s in
  let code = normalize code in
  let code_full = normalize code_full in
  let stderr = normalize stderr in
  let stdout = normalize stdout in
  let mopsa = normalize mopsa in
  let hint = normalize hint in
  "입력 데이터 형식:\n"
  ^ "- Code: 에러가 발생한 코드 블럭\n"
  ^ "- Full Code: 전체 코드\n"
  ^ "- Python Error: 발생한 에러\n"
  ^ "- Python Output: 에러 발생 전까지의 출력\n"
  ^ "- Analysis: 정적분석 결과\n"
  ^ "- Hint: 학생들이 자주 하는 실수 유형"
  ^ "Code: " ^ code ^ "\n\n"
  ^ "Full Code: " ^ code_full ^ "\n\n"
  ^ "Python Error: " ^ stderr ^ "\n\n"
  ^ "Python Output: " ^ stdout ^ "\n\n"
  ^ "Analysis: " ^ mopsa
  ^ "Hint: " ^ hint ^ "\n\n"


let yo_get_opt k = function
  | `Assoc kv -> List.assoc_opt k kv
  | _ -> None

let extract_delta_content (json : Yojson.Safe.t) : string option =
  (* OpenAI/OpenRouter SSE token format:
     {"choices":[{"delta":{"content":"..."} }]} 
     {"choices":[{"message":{"content":"..."} }]}
  *)
  match yo_get_opt "choices" json with
  | Some (`List (choice :: _)) -> begin
      match yo_get_opt "delta" choice with
      | Some (`Assoc _ as delta) ->
          (match yo_get_opt "content" delta with
           | Some (`String s) -> Some s
           | _ -> None)
      | _ ->
          (match yo_get_opt "message" choice with
           | Some (`Assoc _ as msg) ->
               (match yo_get_opt "content" msg with
                | Some (`String s) -> Some s
                | _ -> None)
           | _ -> None)
    end
  | _ -> None


(* callback each event *)
let stream_response
    ~(on_chunk : string -> unit Lwt.t)
    ~(on_error : string -> unit Lwt.t)
    (code : string) (code_full : string) (stderr : string) (mopsa : string) (hint : string)
  =

  (* Call API *)
  let user_input =
    prompt_user ~code ~code_full ~stderr ~mopsa ~hint ()
  in
  let body_json = make_request_json system_input user_input |> Yojson.Safe.to_string in
  let body = Cohttp_lwt.Body.of_string body_json in
  let* (_resp, body_stream) = Cohttp_lwt_unix.Client.post ~headers ~body endpoint in
  let stream = Cohttp_lwt.Body.to_stream body_stream in

  (* Buffer -> SSE Events *)
  let rec pump (buf : string) =
    let* next = Lwt_stream.get stream in
    match next with
    | None ->
        (* Stream Done *)
        Lwt.return_unit
    | Some chunk ->
        let buf = buf ^ chunk in
        (* event divided by "\n\n" or "\r\n\r\n" *)
        let events = String.split_on_char '\n' buf in

        (* event has multi "data: {...}" lines *)
        let rec fold_lines acc cur = function
          | [] ->
              (List.rev acc, String.concat "\n" cur)
          | line :: tl ->
              if String.trim line = "" then
                fold_lines (String.concat "\n" (List.rev cur) :: acc) [] tl
              else
                fold_lines acc (line :: cur) tl
        in
        let completed, rest = fold_lines [] [] events in
        let _ = print_endline ("completed: " ^ (String.concat " " completed)) in

        let handle_event (ev : string) =
          (* "data: ..." -> json parsing *)
          let data_lines =
            ev
            |> String.split_on_char '\n'
            |> List.filter_map (fun l ->
                 if Astring.String.is_prefix ~affix:"data: " l
                 then Some (String.sub l 6 (String.length l - 6))
                 else None)
          in
          Lwt_list.iter_s
            (fun d ->
               if d = "[DONE]" then Lwt.return_unit
               else
                 Lwt.catch
                   (fun () ->
                      let json = Yojson.Safe.from_string d in
                      match extract_delta_content json with
                      | Some chunk_txt -> on_chunk chunk_txt
                      | None -> Lwt.return_unit)
                   (fun exn -> on_error (Printexc.to_string exn)))
            data_lines
        in
        let* () = Lwt_list.iter_s handle_event completed in
        pump rest
  in
  Lwt.catch
    (fun () -> pump "")
    (fun exn -> on_error (Printexc.to_string exn))

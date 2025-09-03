open Lwt.Infix

let api_key =
  match Sys.getenv_opt "OPENROUTER_API_KEY" with
  | Some k when String.trim k <> "" -> k
  | _ -> ""

let model =
  match Sys.getenv_opt "OPENROUTER_MODEL" with
  | Some m when String.trim m <> "" -> m
  | _ -> "meta-llama/llama-4-scout"

let endpoint = Uri.of_string "https://openrouter.ai/api/v1/chat/completions"

let headers =
  Cohttp.Header.of_list
    [
      ("Content-Type", "application/json");
      ("Authorization", "Bearer " ^ api_key);
    ]

let make_request_json user_input =
  `Assoc
    [
      ("model", `String model);
      ("max_tokens", `Int 10000);
      ("stream", `Bool true);
      ("reasoning", `Assoc [ ("exclude", `Bool true)]);
      ( "messages",
        `List
          [
            `Assoc [ ("role", `String "user"); ("content", `String user_input) ];
          ] );
    ]

let prompt ?(code = "") ?(code_full = "") ?(error = "") ?(mopsa = "") ?(hint = "") () =
  let normalize s = if String.trim s = "" then "없음" else s in
  let code = normalize code in
  let code_full = normalize code_full in
  let error = normalize error in
  let mopsa = normalize mopsa in
  let hint = normalize hint in
  "Python 기초 수업을 듣는 학생이 작성한 코드와 그에 대한 정보를 줄게.\n
  Code (에러가 발생한 코드 블럭), Full Code (전체 코드), Python Error (발생한 에러), Analysis (정적분석 결과) 이렇게 4개 정보와 Hint (학생들이 해당 error 발생 시 자주 실수하는 실수 유형)를 알려줄게.
  에러가 발생한 코드 블럭을 수정하는 방법을 쉽게 설명해줘.\n\  "
  ^ "Hint: " ^ hint 
  ^ "Code: " ^ code ^ "\n Full Code:" ^ code_full ^ "\n Python Error: " ^ error ^ "\n Analysis: " ^ mopsa

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
    (code : string) (code_full : string) (error : string) (mopsa : string) (hint : string)
  =

  (* Call API *)
  let user_input =
    prompt ~code ~code_full ~error ~mopsa ~hint ()
  in
  let body_json = make_request_json user_input |> Yojson.Safe.to_string in
  let body = Cohttp_lwt.Body.of_string body_json in
  Cohttp_lwt_unix.Client.post ~headers ~body endpoint
  >>= fun (_resp, body_stream) ->
  let stream = Cohttp_lwt.Body.to_stream body_stream in

  (* Buffer -> SSE Events *)
  let rec pump (buf : string) =
    Lwt_stream.get stream >>= function
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
        Lwt_list.iter_s handle_event completed >>= fun () ->
        pump rest
  in
  Lwt.catch
    (fun () -> pump "")
    (fun exn -> on_error (Printexc.to_string exn))


    (* let call_openrouter user_input =
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

let response code code_full error mopsa =
  let message = prompt ~code ~code_full ~error ~mopsa () in
  call_openrouter message *)
(* Lwt_main.run (call_openrouter message) *)
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

let make_request_json user_input =
  `Assoc
    [
      ("model", `String model);
      ("max_tokens", `Int 10000);
      ("reasoning", `Assoc [ ("exclude", `Bool true) ]);
      ( "messages",
        `List
          [
            `Assoc [ ("role", `String "user"); ("content", `String user_input) ];
          ] );
      ( "response_format",
        `Assoc
          [
            ("type", `String "json_schema");
            ( "json_schema",
              `Assoc
                [
                  ("name", `String "code_fix_feedback");
                  ("strict", `Bool true);
                  ( "schema",
                    `Assoc
                      [
                        ("type", `String "object");
                        ( "properties",
                          `Assoc
                            [
                              ( "fix",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ("description", `String "학생에게 보여줄 고쳐진 코드");
                                  ] );
                              ( "reason",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String "학생에게 보여줄 고쳐진 코드에 대한 쉽고 교육적인 설명"
                                    );
                                  ] );
                              ( "example",
                                `Assoc
                                  [
                                    ("type", `String "string");
                                    ( "description",
                                      `String "학생이 일으킨 실수를 보여줄 수 있는 간결한 예시" );
                                  ] );
                            ] );
                        ( "required",
                          `List
                            [
                              `String "fix"; `String "reason"; `String "example";
                            ] );
                        ("additionalProperties", `Bool false);
                      ] );
                ] );
          ] );
    ]

let prompt code code_full error mopsa =
  let normalize s = if String.trim s = "" then "없음" else s in
  let code = normalize code in
  let code_full = normalize code_full in
  let error = normalize error in
  let mopsa = normalize mopsa in
  "Python 기초 수업을 듣는 학생이 작성한 코드와 그에 대한 정보를 줄게.\n\n\
  \  Code (에러가 발생한 코드 블럭), Full Code (전체 코드), Python Error (발생한 에러), Analysis \
   (정적분석 결과) 이렇게 4개 정보와 학생들이 Error 별로 자주 실수하는 실수 유형을 알려줄게.\n\
  \  에러가 발생한 코드 블럭을 수정하는 방법을 쉽게 설명해줘.\n\
  \  "
  ^ "TypeError 실수 유형\n\
    \  1. 모듈 내 함수 이름 혼동: 함수 이름을 string등 callable하지 않은 타입으로 사용하거나, 그러한 타입의 값을 \
     가진 변수명을 함수명 자리에 사용\n\
    \  2. 내장함수 덮어쓰기: 함수 이름을 callable하지 않은 타입의 변수명으로 지정\n\
    \  3. 모듈의 함수 호출 방법 오류\n\
    \  - 함수 이름만 쓰고 `(인자)`를 붙이지 않아 의도한 연산이 불가능한 function 타입을 가짐\n\
    \  - 함수의 인자 개수와 타입을 잘못 입력\n\
    \  4. lambda 함수 사용법 오류: 정의한 람다 함수의 인자 개수 또는 타입을 잘못 입력\n\
    \  5. dict 자료형 사용법 오류\n\
    \  - `for ... in dict` 구문에서 꺼내는 값이 key가 아니라 value인 것으로 착각함\n\
    \  - values()로 만든 dict_values 전체에 `int()`를 취하는 등 잘못된 자료형을 대상으로 타입 캐스팅\n\
    \  - dict_keys나 dict_values, list 등을 int와 하나하나 비교하지 않고 직접 비교를 수행\n\
    \  - key에 대응하는 value를 `[]`가 아닌 `()`로 찾으려고 시도함" ^ "Code: " ^ code
  ^ "\n Full Code:" ^ code_full ^ "\n Python Error: " ^ error ^ "\n Analysis: "
  ^ mopsa

let call_openrouter user_input =
  let body_json = make_request_json user_input |> Yojson.Safe.to_string in
  let%lwt resp, body_stream =
    Cohttp_lwt_unix.Client.post ~headers
      ~body:(Cohttp_lwt.Body.of_string body_json)
      endpoint
  in
  let%lwt body_str = Cohttp_lwt.Body.to_string body_stream in
  Lwt.return
    (match resp.status with
    | `OK -> body_str
    | status ->
        Printf.sprintf "Error: %s\nResponse: %s"
          (Cohttp.Code.string_of_status status)
          body_str)

let response code code_full error mopsa =
  let message = prompt code code_full error mopsa in
  call_openrouter message

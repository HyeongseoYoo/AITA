(*
TO-DO LIST
1) get full code block & error message
2) make code, code_full, code_simple
2) get *)

let clean s =
    String.to_seq s |> Seq.filter (fun c -> c <> '\r') |> String.of_seq

let code =
  let text =
    {|try:
  scores = {"타블로": 73, "미쓰라": 90, "윤하": 99, "투컷": 82}
except:
  pass

high_scorer = [i for i in scores if i >= 90 ]
print(high_scorer)
|}
  in
  clean text

let code_full =
  let text =
    {|
scores = {"타블로": 73, "미쓰라": 90, "윤하": 99, "투컷": 82}

high_scorer = [i for i in scores if i >= 90 ]
print(high_scorer)
|}
  in
  clean text

let code_simple =
  let text =
    {|high_scorer = [i for i in scores if i >= 90 ]
print(high_scorer)

|}
  in
  text

let error =
  {|
  Traceback (most recent call last):
  File "C:\Workspace\AITA\materials2025\test.py", line 5, in <module>
    high_scorer = [i for i in scores if i >= 90 ]
                                        ^^^^^^^
TypeError: '>=' not supported between instances of 'str' and 'int'
  |}

let hint = 
  let text =
  {|
  TypeError 실수 유형
  1. 모듈 내 함수 이름 혼동: 함수 이름을 string등 callable하지 않은 타입으로 사용하거나, 그러한 타입의 값을 가진 변수명을 함수명 자리에 사용
  2. 내장함수 덮어쓰기: 함수 이름을 callable하지 않은 타입의 변수명으로 지정
  3. 모듈의 함수 호출 방법 오류
  - 함수 이름만 쓰고 `(인자)`를 붙이지 않아 의도한 연산이 불가능한 function 타입을 가짐
  - 함수의 인자 개수와 타입을 잘못 입력
  4. lambda 함수 사용법 오류: 정의한 람다 함수의 인자 개수 또는 타입을 잘못 입력
  5. dict 자료형 사용법 오류
  - `for ... in dict` 구문에서 꺼내는 값이 key가 아니라 value인 것으로 착각함
  - values()로 만든 dict_values 전체에 `int()`를 취하는 등 잘못된 자료형을 대상으로 타입 캐스팅
  - dict_keys나 dict_values, list 등을 int와 하나하나 비교하지 않고 직접 비교를 수행
  - key에 대응하는 value를 `[]`가 아닌 `()`로 찾으려고 시도함
  |} in
  clean text

let error_names = [
  "SyntaxError"; "IndentationError"; "TypeError"; "IndexError"; "AttributeError"; "KeyError";
  "ValueError"; "NameError"; "UnboundLocalError"; "ZeroDivisionError"
]

type res_state = Waiting of string | Explanation | FollowUps

let user_chat_table : (string, int) Hashtbl.t = Hashtbl.create 40 (* session_id |-> num_of_chats *)
let chat_history_table : (string, string) Hashtbl.t = Hashtbl.create 40 (* chat_id |-> chat_history *)

let add_try cell =
  let lines = String.split_on_char '\n' cell in
  let lines = List.map (fun l -> "  " ^ l) lines in
  "try:\n" ^ (String.concat "\n" lines) ^ "\nexcept: pass\n"

let get_error_and_hints (output : Dto.output) =
  let stderr, hint = (
    match output.stderr with
    | Some e ->
        let error_message = String.concat "\n" e in
        let error_name =
          let rec find_name e_list =
            match e_list with
            | [] -> "no error"
            | line :: rest -> (
              match String.index_opt line ':' with
              | Some i ->
                  let name = String.sub line 0 i in
                  if List.mem name error_names then name else find_name rest
              | None -> find_name rest
            )
          in
          find_name e
        in
        let parse_section content =
          let rec find_section head lines =
            match lines with
            | line :: rest when (String.trim line) = head -> rest
            | _ :: rest -> find_section head rest
            | [] -> ["- 힌트를 찾을 수 없음"]
          in
          let rec add_section acc lines =
            match lines with
            | line :: rest when line <> "" -> add_section (line :: acc) rest
            | _ -> List.rev acc
          in
          let lines = String.split_on_char '\n' content in
          find_section error_name lines |> add_section []
        in
        let filename = "lib/data/mistakes.txt" in
        let content =
          try
            In_channel.with_open_text filename In_channel.input_all
          with
          | Sys_error _ -> "error raised while reading text file" in
        error_message, String.concat "\n" (parse_section content)
    | None -> "", ""
  ) in
  let stdout = (
    match output.stdout with
    | Some o -> String.concat "\n" o
    | None -> ""
  ) in
  let result = (
    match output.result with
    | Some r -> String.concat "\n" r
    | None -> ""
  ) in
  stderr, hint, ("stdout: " ^ stdout ^ "\nresult: " ^ result)
  

let chat_id_of session_id =
  let num_of_chats = Hashtbl.find user_chat_table session_id in
  let _ = Hashtbl.replace user_chat_table session_id (num_of_chats + 1) in
  session_id ^ "-" ^ (string_of_int (num_of_chats + 1))

type parser_state =
  | Idle
  | InExplanation
  | SeekingFollowUps
  | InFollowUps

type streaming_parser = {
  mutable buffer: string;
  mutable state: parser_state;
  mutable explanation_value: string;
  mutable followup_value: string list;
  mutable last_processed_pos: int;
}

let chat_parser_table : (string, streaming_parser) Hashtbl.t = Hashtbl.create 40

let create_streaming_parser () = {
  buffer = "";
  state = Idle;
  explanation_value = "";
  followup_value = [];
  last_processed_pos = 0;
}

(* 필드 이름이 field인 객체의 값이 시작하는 위치를 찾아 반환 *)
let find_field_start s field pos = 
  let pattern = "\"" ^ field ^ "\"" ^ "[ 	
]*:" in
  try
    let re = Str.regexp pattern in
    let _start_pos = Str.search_forward re s pos in
    Some (Str.match_end ())
  with Not_found -> None

let rec skip_whitespace s i = 
  if i >= String.length s then i
  else match s.[i] with 
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace s (i + 1)
  | _ -> i

(* s를 start_pos 위치부터 탐색하여 처음으로 오는 ""로 묶인 텍스트와 바로 다음 위치를 반환 *)
let parse_string s start_pos = 
  let len = String.length s in
  let p = skip_whitespace s start_pos in
  if p >= len || s.[p] <> '"' then None
  else
    let buf = Buffer.create (len - p) in
    let rec loop i = 
      if i >= len then None (* String not terminated *)
      else match s.[i] with
      | '\\' -> 
          if i + 1 < len then (
            (match s.[i+1] with
            | 'n' -> Buffer.add_char buf '\n'
            | 'r' -> Buffer.add_char buf '\r'
            | 't' -> Buffer.add_char buf '\t'
            | c -> Buffer.add_char buf c);
            loop (i + 2)
          ) else None (* Trailing backslash *)
      | '"' -> Some (Buffer.contents buf, i + 1)
      | c -> 
          Buffer.add_char buf c;
          loop (i + 1)
    in 
    loop (p + 1)

let extract_streaming_chunk parser chunk = 
  parser.buffer <- parser.buffer ^ chunk;
  let len = String.length parser.buffer in
  let output = ref "" in

  let rec process_buffer () = 
    if parser.last_processed_pos >= len then () 
    else match parser.state with
    | Idle -> 
        (match find_field_start parser.buffer "explanation" parser.last_processed_pos with
        | Some pos -> 
            parser.state <- InExplanation;
            parser.last_processed_pos <- pos;
            process_buffer ()
        | None -> ())
    | InExplanation -> 
        (match parse_string parser.buffer parser.last_processed_pos with 
        | Some (value, next_pos) -> 
            if value <> parser.explanation_value then
              output := Yojson.Safe.to_string (`Assoc ["explanation", `String value]);
              parser.explanation_value <- value;
            
            parser.last_processed_pos <- next_pos;
            parser.state <- SeekingFollowUps;
            process_buffer ()
        | None -> 
            let p = skip_whitespace parser.buffer parser.last_processed_pos in
            if p < len && parser.buffer.[p] = '"' then
              let partial_content_escaped = String.sub parser.buffer (p + 1) (len - (p + 1)) in
              let partial_content_unescaped = 
                let temp_buf = Buffer.create (String.length partial_content_escaped) in
                let rec loop i = 
                  if i < String.length partial_content_escaped then
                    match partial_content_escaped.[i] with 
                    | '\\' -> 
                        if i + 1 < String.length partial_content_escaped then (
                          (match partial_content_escaped.[i+1] with
                          | 'n' -> Buffer.add_char temp_buf '\n'
                          | 'r' -> Buffer.add_char temp_buf '\r'
                          | 't' -> Buffer.add_char temp_buf '\t'
                          | c -> Buffer.add_char temp_buf c);
                          loop (i + 2)
                        ) else () 
                    | c -> Buffer.add_char temp_buf c; loop (i + 1)
                in loop 0;
                Buffer.contents temp_buf
              in
              if partial_content_unescaped <> parser.explanation_value then
                output := Yojson.Safe.to_string (`Assoc ["explanation", `String partial_content_unescaped]);
                parser.explanation_value <- partial_content_unescaped
        )
    | SeekingFollowUps -> 
        (match find_field_start parser.buffer "followUps" parser.last_processed_pos with
        | Some pos -> 
            parser.state <- InFollowUps;
            parser.last_processed_pos <- pos;
            process_buffer ()
        | None -> ())
    | InFollowUps -> 
        let p = skip_whitespace parser.buffer parser.last_processed_pos in
        if p < len && parser.buffer.[p] = '[' then 
          let rec parse_items pos = 
            let current_pos = skip_whitespace parser.buffer pos in 
            if current_pos < len then 
              match parser.buffer.[current_pos] with 
              | ']' -> parser.last_processed_pos <- current_pos + 1
              | ',' -> parse_items (current_pos + 1)
              | '"' -> 
                  (match parse_string parser.buffer current_pos with 
                  | Some (item, next_pos) -> 
                      if not (List.mem item parser.followup_value) then (
                        parser.followup_value <- parser.followup_value @ [item];
                        output := Yojson.Safe.to_string
                          (`Assoc [("followUps", `List (List.map (fun s -> `String s) parser.followup_value))])
                      );
                      parse_items next_pos
                  | None -> ())
              | _ -> () 
            else () 
          in 
          parse_items (p + 1)
  in 

  process_buffer ();
  !output

let response_to_json chunk chat_id = 
  let parser = Hashtbl.find chat_parser_table chat_id in
  let result = extract_streaming_chunk parser chunk in
  if result = "" then "{}\n" else result ^ "\n"

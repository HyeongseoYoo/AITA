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

(* let extract (json_str : string) : string =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields -> (
        (* Prefer OpenRouter/OpenAI chat-completions shape: choices[0].message.content *)
        match List.assoc_opt "choices" fields with
        | Some (`List (choice :: _)) -> (
            match choice with
            | `Assoc choice_fields -> (
                match List.assoc_opt "message" choice_fields with
                | Some (`Assoc msg_fields) -> (
                    match List.assoc_opt "content" msg_fields with
                    | Some (`String text) -> text
                    | Some (`List parts) ->
                        (* Join any string parts if present; fallback to failure *)
                        let collect acc part =
                          match part with
                          | `String s -> acc ^ s
                          | `Assoc assoc -> (
                              match List.assoc_opt "text" assoc with
                              | Some (`String s) -> acc ^ s
                              | _ -> acc)
                          | _ -> acc
                        in
                        let text = List.fold_left collect "" parts in
                        if String.trim text = "" then "분석 실패" else text
                    | _ -> "분석 실패")
                | _ -> (
                    (* Some providers may return choices[0].text *)
                    match List.assoc_opt "text" choice_fields with
                    | Some (`String text) -> text
                    | _ -> "분석 실패"))
            | _ -> "분석 실패")
        | _ -> (
            (* Backward-compat: Anthropic messages shape used previously *)
            match List.assoc_opt "content" fields with
            | Some (`List (`Assoc item :: _)) -> (
                match List.assoc_opt "text" item with
                | Some (`String text) -> text
                | _ -> "분석 실패")
            | _ -> (
                (* Fallback to error message if present *)
                match List.assoc_opt "error" fields with
                | Some (`Assoc err_fields) -> (
                    match List.assoc_opt "message" err_fields with
                    | Some (`String msg) -> msg
                    | _ -> "분석 실패")
                | _ -> "분석 실패")))
    | _ -> "분석 실패"
  with _ -> "분석 실패"
 *)

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
  let clean s =
    String.to_seq s |> Seq.filter (fun c -> c <> '\r') |> String.of_seq
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

let extract (json_str : string) : string =
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

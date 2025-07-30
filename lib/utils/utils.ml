let code =
  let text =
{|try:
  scores = {"타블로": 73, "미쓰라": 90, "윤하": 99, "투컷": 82}
except:
  pass

high_scorer = [i for i in scores if i >= 90 ]
print(high_scorer)
  
|} in
  let clean s = String.to_seq s
  |> Seq.filter (fun c -> c <> '\r')
  |> String.of_seq in
  clean text

let code_simple =
  let text =
{|high_scorer = [i for i in scores if i >= 90 ]
print(high_scorer)

|} in
text


let error = {|
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
    | `Assoc fields ->
        begin match List.assoc_opt "content" fields with
        | Some (`List (`Assoc item :: _)) ->  (* content[0] 항목 *)
            begin match List.assoc_opt "text" item with
            | Some (`String text) -> text
            | _ -> "분석 실패"
            end
        | _ -> "분석 실패"
        end
    | _ -> "분석 실패"
  with _ -> "분석 실패"
(* POST /login *)
type login_response_body = { session_id : string } [@@deriving yojson]

(* POST /analysis *)
type context_cell = { cell_id : string; code : string } [@@deriving yojson]

type analysis_request_body = {
  session_id : string;
  cell_id : string;
  context : context_cell list;
  code : string;
}
[@@deriving yojson]

type analysis_response_body = {
  explanation : string;
  follow_ups : string list; [@key "followUps"]
}
[@@deriving yojson]

(* POST /chat *)
type chat_request_body = { chat_id : string; prompt : string }
[@@deriving yojson]

type chat_response_body = {
  explanation : string;
  follow_ups : string list; [@key "followUps"]
}
[@@deriving yojson]

(* Common *)
type error = { code : string; message : string } [@@deriving yojson]

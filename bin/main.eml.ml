let server_secret_key =
  match Sys.getenv "AITA_PASSWD" |> String.trim with
  | k when k <> "" -> k
  | _ | (exception Not_found) -> failwith "AITA_PASSWD is not set"

let login_page error_message =
  <html>
  <body>
    <h1>API Key Access</h1>
    <p>실행을 위해 키를 입력해주세요.</p>
    <p style="color:red;"><%s error_message %></p>
    <form method="POST" action="/">
      <input type="password" name="secret_key" placeholder="Enter key here" size="50">
      <button type="submit">Submit</button>
    </form>
  </body>
  </html>

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream.html (login_page "" ));

         Dream.post "/" (fun request ->
             match%lwt Dream.form ~csrf:false request with
             | `Ok ["secret_key", user_key] when user_key = server_secret_key ->
                 let code = Utils.code in
                 let code_simple = Utils.code_simple in
                 let code_full = Utils.code_full in
                 let error = Utils.error in
                 let exitcode, output = Analysis.run_analysis code in
                 let mopsa = if exitcode = 1 then output else "" in
                 let%lwt llm_output = Llm.response code_simple code_full error mopsa in
                 Dream.html llm_output

             | `Ok _ ->
                 Dream.html ~status:`Forbidden (login_page "키가 올바르지 않습니다.")

             | _ ->
                 Dream.html ~status:`Bad_Request "잘못된 요청입니다."
            );
       ]

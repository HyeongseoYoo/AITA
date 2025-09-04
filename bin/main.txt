open Lwt.Infix

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             let code = Utils.code in
             let code_simple = Utils.code_simple in
             let code_full = Utils.code_full in
             let error = Utils.error in
             let hint = Utils.hint in
             let exitcode, output = Analysis.run_analysis code in
             let mopsa = if exitcode = 1 then output else "" in

             Dream.stream
             ~headers:[("Content-Type", "text/html; charset=utf-8")]
             (fun s ->
               (* 1) Initial HTML frame *)
               let%lwt () =
                 Dream.write s
                   "<!doctype html><meta charset='utf-8'>\
                    <style>body{font-family:ui-monospace,monospace;white-space:pre-wrap}\
                    .dim{opacity:.6}</style>\
                    <h1>AITA (Streaming ver.) </h1><div id='out'>"
               in
               (* 2) Call back - Print Normal token *)
               let on_chunk (txt : string) =
                 Dream.write s (Dream.html_escape txt) >>= fun () ->
                 Dream.flush s
               in
               (* 3) Call back - Print Error *)
               let on_error (msg : string) =
                 Dream.write s
                   ("\n\n<hr><strong class='dim'>Error:</strong> "
                    ^ Dream.html_escape msg)
               in
               (* 4) OpenRouter Request with streaming → SSE token -> on_chunk / on_error *)
               Llm.stream_response
                 ~on_chunk ~on_error
                 code_simple code_full error mopsa hint
               >>= fun () ->

               (* 5) Final tag and flush *)
               Dream.write s "</div>"));
       ]

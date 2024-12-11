open Opium

let hello_handler _req = 
  Response.of_plain_text "Hello, World!" |> Lwt.return

let json_handler _req =
  let json = `Assoc [ ("message", `String "Hello, JSON!") ] in
  Response.of_json json |> Lwt.return

let param_handler req =
  let name = Router.param req "name" in
  Response.of_plain_text ("Hello, " ^ name ^ "!") |> Lwt.return

let () =
  App.empty
  |> App.get "/" hello_handler
  |> App.get "/json" json_handler
  |> App.get "/hello/:name" param_handler
  |> App.run_command

(*lone
CORS
let cors = Rock.Middleware.create ~name:"cors" (fun handler req ->
  handler req >|= fun response ->
  Response.add_header response "Access-Control-Allow-Origin" "*"
  |> Response.add_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
  |> Response.add_header "Access-Control-Allow-Headers" "Content-Type")

let () =
  App.empty
  |> App.middleware cors
  |> App.get "/" hello_handler
  |> App.run_command

POST
let post_handler req =
  let open Lwt.Syntax in
  let* json = Request.to_json_exn req in
  match json with
  | `Assoc ["message", `String message] ->
      Response.of_json (`Assoc ["echo", `String message]) |> Lwt.return
  | _ ->
      Response.of_plain_text ~status:`Bad_request "Invalid JSON" |> Lwt.return

let () =
  App.empty
  |> App.post "/echo" post_handler
  |> App.run_command

Static
let () =
  App.empty
  |> App.middleware (Static.middleware ~local_path:"./static" ~uri_prefix:"/static")
  |> App.run_command

with static/index.html

*)

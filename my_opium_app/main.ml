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
open Opium
open Lwt.Syntax

let json_of_verification_result = function
  | Valid -> `Assoc [("status", `String "valid")]
  | Invalid msg -> `Assoc [
      ("status", `String "invalid");
      ("message", `String msg)
    ]
  | Error msg -> `Assoc [
      ("status", `String "error");
      ("message", `String msg)
    ]

let verify_policy_handler req =
  let* json = Request.to_json_exn req in
  let policy = policy_of_json json in
  let result = PolicyVerifier.verify_permission_consistency policy in
  let result = match result with
    | Valid -> PolicyVerifier.verify_mutual_exclusion policy
    | _ -> result
  in
  Response.of_json (json_of_verification_result result)
  |> Lwt.return

let verify_crypto_handler req =
  let* json = Request.to_json_exn req in
  let protocol = protocol_of_json json in
  let result = CryptoVerifier.verify_protocol protocol in
  Response.of_json (json_of_verification_result result)
  |> Lwt.return

let () =
  App.empty
  |> App.post "/verify/policy" verify_policy_handler
  |> App.post "/verify/crypto" verify_crypto_handler
  |> App.run_command

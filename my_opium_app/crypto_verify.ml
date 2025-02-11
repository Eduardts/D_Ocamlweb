module CryptoVerifier = struct
  type protocol_step = 
    | Encrypt of string * string  (* data, key *)
    | Decrypt of string * string
    | Hash of string
    | Sign of string * string
    | Verify of string * string

  type protocol = {
    name: string;
    steps: protocol_step list;
    security_properties: security_property list;
  }

  and security_property =
    | Confidentiality of string
    | Integrity of string
    | Authentication of string * string

  let verify_protocol protocol =
    let state = ref (Hashtbl.create 16) in
    try
      List.iter (fun step ->
        match step with
        | Encrypt (data, key) ->
            if not (is_key_secure !state key) then
              raise (Invalid_argument "Key security violation")
        | Decrypt (data, key) ->
            if not (was_encrypted !state data) then
              raise (Invalid_argument "Attempting to decrypt unencrypted data")
        | Hash data ->
            Hashtbl.add !state data "hashed"
        | Sign (data, key) ->
            if not (is_key_secure !state key) then
              raise (Invalid_argument "Signing key security violation")
        | Verify (sig_, key) ->
            if not (was_signed !state sig_) then
              raise (Invalid_argument "Invalid signature verification")
      ) protocol.steps;
      Valid
    with
    | Invalid_argument msg -> Invalid msg
    | e -> Error (Printexc.to_string e)
end


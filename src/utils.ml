module Auth = struct
  open Jose

  exception Invalid_jwt_token
  exception Fail_to_generate_jwt of string

  type payload = { user_id : string; expires : int } [@@deriving yojson]

  let jwk = Jwk.make_oct "SECRET"

  let verify token =
    let now = Ptime_clock.now () in
    let jwt_token = Jwt.unsafe_of_string token |> Result.get_ok in

    match Jwt.validate ~jwk ~now jwt_token with
    | Result.Ok e -> Some e
    | Result.Error _ -> None

  let generate_jwt_token user_id =
    let expires = int_of_float (Unix.time ()) + (60 * 60) in
    let payload = payload_to_yojson { user_id; expires } in

    match Jwt.sign ~payload jwk with
    | Result.Ok token -> Jwt.to_string token
    | Result.Error (`Msg error) -> raise @@ Fail_to_generate_jwt error
end

open Opium

let ( let* ) = Lwt.bind

type response_error = { message : string } [@@deriving yojson]

module User = struct
  type user_request_login = { email : string; password : string }
  [@@deriving yojson]

  type user_response = { id : string; name : string; email : string }
  [@@deriving yojson]

  type user_response_ok = {
    message : string;
    token : string;
    user : user_response;
  }
  [@@deriving yojson]

  let register (req : Rock.Request.t) =
    let* body = Request.to_json_exn req in
    let parsed_user =
      match Storage.User.user_of_yojson body with
      | Ok user -> user
      | Error error -> raise (Invalid_argument error)
    in
    let* { id; name; email; password = _ } = Storage.User.insert parsed_user in
    Lwt.return
      (Response.of_json
      @@ user_response_ok_to_yojson
           {
             message = "Successfully registered!";
             user = { id; name; email };
             token = Utils.Auth.generate_jwt_token id;
           })

  let login (req : Rock.Request.t) =
    let* body = Request.to_json_exn req in
    let parsed_user =
      match user_request_login_of_yojson body with
      | Ok user -> user
      | Error error -> raise (Invalid_argument error)
    in
    let* user = Storage.User.find_by_email parsed_user.email in
    match user with
    | Some { id; name; email; password } ->
        let is_password_correct =
          let password_hashed = Bcrypt.hash_of_string password in
          Bcrypt.verify parsed_user.password password_hashed
        in
        if is_password_correct then
          Lwt.return
            (Response.of_json
            @@ user_response_ok_to_yojson
                 {
                   message = "Successfully logged in!";
                   user = { id; name; email };
                   token = Utils.Auth.generate_jwt_token id;
                 })
        else
          Lwt.return
            (Response.of_json
             @@ response_error_to_yojson { message = "Password is incorrect!" }
            |> Response.set_status `Unauthorized)
    | None ->
        Lwt.return
          (Response.of_json
           @@ response_error_to_yojson { message = "User not found!" }
          |> Response.set_status `Not_found)
end

module Middleware = struct
  let auth_middleware app =
    let key : string Context.key =
      Context.Key.create ("user", Sexplib.Std.sexp_of_string)
    in

    let filter handler (req : Rock.Request.t) =
      match Request.header "Authorization" req with
      | Some token -> (
          match Utils.Auth.verify token with
          | Some jwt ->
              let payload =
                Utils.Auth.payload_of_yojson jwt.payload |> Result.get_ok
              in
              let* user = Storage.User.find_by_id payload.user_id in
              let user =
                Option.get user |> Storage.User.user_stored_to_yojson
              in
              let env =
                Context.add key (Yojson.Safe.show user) req.Request.env
              in
              handler { req with env }
          | None ->
              Lwt.return
                (Response.of_json
                 @@ response_error_to_yojson
                      { message = "User not authenticaded" }
                |> Response.set_status `Unauthorized))
      | None ->
          Lwt.return
            (Response.of_json
             @@ response_error_to_yojson { message = "User not authenticaded" }
            |> Response.set_status `Unauthorized)
    in

    let m = Rock.Middleware.create ~name:"authentication" ~filter in
    Rock.Middleware.apply m app
end

(* module Card = struct end *)

let app =
  App.empty
  |> App.get "/fafa"
     @@ Middleware.auth_middleware (fun _ ->
            Lwt.return (Response.of_plain_text "Hello, World!"))
  |> App.post "/login" User.login
  |> App.post "/register" User.register

let () = app |> App.run_multicore

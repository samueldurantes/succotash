let ( let* ) = Lwt.bind

module Migrations = struct
  let ensure_users_table_exists =
    [%rapper
      execute
        {sql|
          CREATE TABLE IF NOT EXISTS users (
            id       UUID PRIMARY KEY NOT NULL,
            name     TEXT NOT NULL,
            email    TEXT NOT NULL UNIQUE,
            password TEXT NOT NULL
          );
        |sql}]
      ()

  let ensure_cards_table_exists =
    [%rapper
      execute
        {sql|
          CREATE TABLE IF NOT EXISTS cards (
            id          UUID PRIMARY KEY NOT NULL,
            value       INT NOT NULL,
            status      TEXT NOT NULL,
            created_by  UUID NOT NULL,
            FOREIGN KEY (created_by) REFERENCES users(id)
          );
        |sql}]
      ()
end

module Database = struct
  module type DB = Rapper_helper.CONNECTION

  exception Query_failed of string

  let pool =
    let connection_url = "postgres://samueldurante@localhost/suco" in
    match
      Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url)
    with
    | Ok pool ->
        Logs.info (fun m -> m "Database connected");
        pool
    | Error error -> failwith @@ Caqti_error.show error

  let dispatch f =
    let* result = Caqti_lwt.Pool.use f pool in
    match result with
    | Ok data -> Lwt.return data
    | Error error -> Lwt.fail @@ Query_failed (Caqti_error.show error)

  let () = Lwt_main.run @@ dispatch Migrations.ensure_users_table_exists
  let () = Lwt_main.run @@ dispatch Migrations.ensure_cards_table_exists
end

module User = struct
  type user = { name : string; email : string; password : string }
  [@@deriving yojson]

  type user_stored = {
    id : string;
    name : string;
    email : string;
    password : string;
  }
  [@@deriving yojson]

  let find_by_id id =
    let read_one =
      [%rapper
        get_opt
          {sql|
              SELECT @string{id}, @string{name}, @string{email}, @string{password}
              FROM users
              WHERE id = %string{id}
            |sql}
          record_out]
    in
    let* user = Database.dispatch (read_one ~id) in
    match user with
    | Some user -> Lwt.return @@ Some user
    | None -> Lwt.return None

  let find_by_email email =
    let read_one =
      [%rapper
        get_opt
          {sql|
            SELECT @string{id}, @string{name}, @string{email}, @string{password}
            FROM users
            WHERE email = %string{email}
          |sql}
          record_out]
    in
    let* user = Database.dispatch (read_one ~email) in
    match user with
    | Some user -> Lwt.return @@ Some user
    | None -> Lwt.return None

  let insert ({ name; email; password } : user) =
    let insert =
      [%rapper
        execute
          {sql|
            INSERT INTO users
            VALUES(%string{id}, %string{name}, %string{email}, %string{password})
          |sql}
          record_in]
    in
    let user =
      let id = Uuidm.create `V4 |> Uuidm.to_string in
      let password = Bcrypt.hash password |> Bcrypt.string_of_hash in
      { id; name; email; password }
    in
    let* () = Database.dispatch (insert user) in
    Lwt.return user
end

module Card = struct
  type card = { value : int; status : string; created_by : string }
  [@@deriving yojson]

  type card_stored = {
    id : string;
    value : int;
    status : string;
    created_by : string;
  }
  [@@deriving yojson]

  let find_by_id id =
    let read_one =
      [%rapper
        get_opt
          {sql|
              SELECT @string{id}, @int{value}, @string{status}, @string{created_by}
              FROM cards
              WHERE id = %string{id}
            |sql}
          record_out]
    in
    let* card = Database.dispatch (read_one ~id) in
    match card with
    | Some card -> Lwt.return @@ Some card
    | None -> Lwt.return None

  let insert ({ value; status; created_by } : card) =
    let insert =
      [%rapper
        execute
          {sql|
              INSERT INTO cards
              VALUES(%string{id}, %int{value}, %string{status}, %string{created_by})
            |sql}
          record_in]
    in
    let user =
      let id = Uuidm.create `V4 |> Uuidm.to_string in
      (* TODO: create an ADT to status *)
      { id; value; status; created_by }
    in
    let* () = Database.dispatch (insert user) in
    Lwt.return user
end

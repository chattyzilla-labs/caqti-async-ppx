type a = {username: string};
type b = {
  id: int,
  username: string,
};
type c = {
  id: int,
  username: string,
  email: string,
};
let many_arg_execute = {
  let query =
    Caqti_request.(exec)(
      [@ocaml.warning "-33"]
      Caqti_type.(tup2(string, tup2(string, tup2(option(string), int)))),
      "\n      UPDATE users\n      SET (username, email, bio) = (?, ?, ?)\n      WHERE id = ?\n      ",
    );
  let wrapped =
      (~username, ~email, ~bio, ~id, module Db: Caqti_async.CONNECTION) =>
    Db.exec(query, (username, (email, (bio, id))));
  wrapped;
};
let single_arg_execute = {
  let query =
    Caqti_request.(exec)(
      [@ocaml.warning "-33"] Caqti_type.(string),
      "\n      UPDATE users\n      SET username = ?\n      ",
    );
  let wrapped = (~username, module Db: Caqti_async.CONNECTION) =>
    Db.exec(query, username);
  wrapped;
};
let no_arg_execute = {
  let query =
    Caqti_request.(exec)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      "\n      UPDATE users\n      SET username = 'Hello!'\n      ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) =>
    Db.exec(query, ());
  wrapped;
};
let many_arg_get_one = {
  let query =
    Caqti_request.(find)(
      [@ocaml.warning "-33"] Caqti_type.(tup2(string, int)),
      [@ocaml.warning "-33"]
      Caqti_type.(tup2(int, tup2(string, tup2(option(string), bool)))),
      "\n      SELECT id, username, bio, is_married\n      FROM users\n      WHERE username = ? AND id > ?\n      ",
    );
  let wrapped = (~username, ~min_id, module Db: Caqti_async.CONNECTION) => {
    let f = result =>
      switch (result) {
      | Ok(id, (username, (bio, is_married))) =>
        Ok(id, username, bio, is_married)
      | Error(e) => Error(e)
      };
    Async_kernel.Deferred.map(Db.find(query, (username, min_id)), ~f);
  };
  wrapped;
};
let single_arg_get_one = {
  let query =
    Caqti_request.(find)(
      [@ocaml.warning "-33"] Caqti_type.(string),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      ",
    );
  let wrapped = (~username, module Db: Caqti_async.CONNECTION) => {
    let f = result =>
      switch (result) {
      | Ok(id, username) => Ok({id, username})
      | Error(e) => Error(e)
      };
    Async_kernel.Deferred.map(Db.find(query, username), ~f);
  };
  wrapped;
};
let no_arg_get_one = {
  let query =
    Caqti_request.(find)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, tup2(string, string))),
      "\n      SELECT id, username, email\n      FROM users\n      ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) => {
    let f = result =>
      switch (result) {
      | Ok(id, (username, email)) => Ok({id, username, email})
      | Error(e) => Error(e)
      };
    Async_kernel.Deferred.map(Db.find(query, ()), ~f);
  };
  wrapped;
};
let many_arg_get_one_repeated_arg = {
  let query =
    Caqti_request.(find)(
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, tup2(string, int))),
      [@ocaml.warning "-33"] Caqti_type.(string),
      "\n      SELECT username\n      FROM users\n      WHERE id = ? OR username = ? OR id <> ?\n      ",
    );
  let wrapped = (~id, ~username, module Db: Caqti_async.CONNECTION) => {
    let f = result =>
      switch (result) {
      | Ok(username) => Ok({username: username})
      | Error(e) => Error(e)
      };
    Async_kernel.Deferred.map(Db.find(query, (id, (username, id))), ~f);
  };
  wrapped;
};
let many_arg_get_opt = {
  let query =
    Caqti_request.(find_opt)(
      [@ocaml.warning "-33"] Caqti_type.(tup2(string, int)),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ? AND id > ?\n      ",
    );
  let wrapped = (~username, ~min_id, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => (id, username);
      let f =
        (
          (f, x) =>
            switch (x) {
            | Some(x) => Some(f(x))
            | None => None
            }
        )(
          g,
        );
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.find_opt(query, (username, min_id)), ~f);
  };
  wrapped;
};
let single_arg_get_opt = {
  let query =
    Caqti_request.(find_opt)(
      [@ocaml.warning "-33"] Caqti_type.(string),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      ",
    );
  let wrapped = (~username, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => {id, username};
      let f =
        (
          (f, x) =>
            switch (x) {
            | Some(x) => Some(f(x))
            | None => None
            }
        )(
          g,
        );
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.find_opt(query, username), ~f);
  };
  wrapped;
};
let no_arg_get_opt = {
  let query =
    Caqti_request.(find_opt)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => (id, username);
      let f =
        (
          (f, x) =>
            switch (x) {
            | Some(x) => Some(f(x))
            | None => None
            }
        )(
          g,
        );
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.find_opt(query, ()), ~f);
  };
  wrapped;
};
let many_arg_get_many = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(tup2(string, int)),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ? AND id > ?\n      ",
    );
  let wrapped = (~username, ~min_id, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => {id, username};
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(
      Db.collect_list(query, (username, min_id)),
      ~f,
    );
  };
  wrapped;
};
let single_arg_get_many = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(string),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      WHERE username = ?\n      ",
    );
  let wrapped = (~username, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => (id, username);
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, username), ~f);
  };
  wrapped;
};
let no_arg_get_many = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, username\n      FROM users\n      ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, username)) => {id, username};
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, ()), ~f);
  };
  wrapped;
};
let my_query = {
  let query =
    Caqti_request.(find_opt)(
      [@ocaml.warning "-33"] Caqti_type.(tup2(string, int)),
      [@ocaml.warning "-33"]
      Caqti_type.(tup2(int, tup2(string, tup2(bool, option(string))))),
      "\n      SELECT id, username, following, bio\n      FROM users\n      WHERE username <> ? AND id > ?\n      ",
    );
  let wrapped = (~wrong_user, ~min_id, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, (username, (following, bio)))) => (
        id,
        username,
        following,
        bio,
      );
      let f =
        (
          (f, x) =>
            switch (x) {
            | Some(x) => Some(f(x))
            | None => None
            }
        )(
          g,
        );
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.find_opt(query, (wrong_user, min_id)), ~f);
  };
  wrapped;
};
let list = {
  let wrapped = (~following, ~ids, module Db: Caqti_async.CONNECTION) =>
    switch (ids) {
    | [] =>
      Async_kernel.Deferred.Result.fail(
        Caqti_error.(
          encode_rejected(
            ~uri=Uri.empty,
            ~typ=Caqti_type.unit,
            Msg("Empty list"),
          )
        ),
      )
    | elems =>
      let subsqls = Stdlib.List.map(_ => "?", elems);
      let patch = Stdlib.String.concat(", ", subsqls);
      let sql =
        "\n      SELECT id, username, following, bio\n      FROM users\n      WHERE following = ? and username IN ("
        ++ patch
        ++ ")\n      ";
      open Rapper.Internal;
      let Dynparam.Pack(packed_list_type, ids) =
        Stdlib.List.fold_left(
          (pack, item) =>
            Dynparam.add([@ocaml.warning "-33"] Caqti_type.(int), item, pack),
          Dynparam.empty,
          elems,
        );
      let query =
        Caqti_request.(find_opt)(
          ~oneshot=true,
          Caqti_type.(tup2(bool, packed_list_type)),
          [@ocaml.warning "-33"]
          Caqti_type.(tup2(int, tup2(string, tup2(bool, option(string))))),
          sql,
        );
      let f = result => {
        let g = ((id, (username, (following, bio)))) => (
          id,
          username,
          following,
          bio,
        );
        let f =
          (
            (f, x) =>
              switch (x) {
              | Some(x) => Some(f(x))
              | None => None
              }
          )(
            g,
          );
        switch (result) {
        | Ok(x) => Ok(f(x))
        | Error(e) => Error(e)
        };
      };
      Async_kernel.Deferred.map(Db.find_opt(query, (following, ids)), ~f);
    };
  wrapped;
};
let collect_list = {
  let wrapped = (~versions, module Db: Caqti_async.CONNECTION) =>
    switch (versions) {
    | [] =>
      Async_kernel.Deferred.Result.fail(
        Caqti_error.(
          encode_rejected(
            ~uri=Uri.empty,
            ~typ=Caqti_type.unit,
            Msg("Empty list"),
          )
        ),
      )
    | elems =>
      let subsqls = Stdlib.List.map(_ => "?", elems);
      let patch = Stdlib.String.concat(", ", subsqls);
      let sql =
        " SELECT id from schema_migrations where version in (" ++ patch ++ ")";
      open Rapper.Internal;
      let Dynparam.Pack(packed_list_type, versions) =
        Stdlib.List.fold_left(
          (pack, item) =>
            Dynparam.add([@ocaml.warning "-33"] Caqti_type.(int), item, pack),
          Dynparam.empty,
          elems,
        );
      let query =
        Caqti_request.(collect)(
          ~oneshot=true,
          packed_list_type,
          [@ocaml.warning "-33"] Caqti_type.(string),
          sql,
        );
      Db.collect_list(query, versions);
    };
  wrapped;
};
module Suit: Rapper.CUSTOM = {
  type t =
    | Clubs
    | Diamonds
    | Hearts
    | Spades;
  let t = {
    let encode =
      fun
      | Clubs => Ok("c")
      | Diamonds => Ok("d")
      | Hearts => Ok("h")
      | Spades => Ok("s");
    let decode =
      fun
      | "c" => Ok(Clubs)
      | "d" => Ok(Diamonds)
      | "h" => Ok(Hearts)
      | "s" => Ok(Spades)
      | _ => Error("invalid suit");
    Caqti_type.(custom(~encode, ~decode, string));
  };
};
let get_cards = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(Suit.t),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, Suit.t)),
      " SELECT id, suit FROM cards WHERE suit <> ? ",
    );
  let wrapped = (~suit, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, suit)) => (id, suit);
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, suit), ~f);
  };
  wrapped;
};
let all_types = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"]
      Caqti_type.(
        tup2(
          string,
          tup2(
            octets,
            tup2(
              int,
              tup2(
                int32,
                tup2(
                  int64,
                  tup2(
                    bool,
                    tup2(
                      float,
                      tup2(
                        pdate,
                        tup2(
                          ptime,
                          tup2(
                            ptime_span,
                            tup2(
                              Caqti_type_calendar.cdate,
                              Caqti_type_calendar.ctime,
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        )
      ),
      " SELECT id, payload, version,\n                some_int32, some_int64, added,\n                fl, date, time, span,\n                cd, ct\n         FROM some_table ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g =
          (
            (
              id,
              (
                payload,
                (
                  version,
                  (
                    some_int32,
                    (
                      some_int64,
                      (added, (fl, (date, (time, (span, (cd, ct)))))),
                    ),
                  ),
                ),
              ),
            ),
          ) => (
        id,
        payload,
        version,
        some_int32,
        some_int64,
        added,
        fl,
        date,
        time,
        span,
        cd,
        ct,
      );
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, ()), ~f);
  };
  wrapped;
};
module Nested = {
  module Suit = Suit;
};
let get_cards = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(Nested.Suit.t),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, Nested.Suit.t)),
      " SELECT id, suit FROM cards WHERE suit <> ? ",
    );
  let wrapped = (~suit, module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, suit)) => (id, suit);
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, suit), ~f);
  };
  wrapped;
};
type user = {
  user_id: int,
  name: string,
};
type twoot = {
  twoot_id: int,
  content: string,
  likes: int,
};
let get_multiple_record_out = {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"]
      Caqti_type.(tup2(int, tup2(string, tup2(int, tup2(string, int))))),
      "\n      SELECT users.user_id, users.name,\n             twoots.twoot_id, twoots.content, twoots.likes\n      FROM users\n      JOIN twoots ON twoots.user_id = users.user_id\n      ORDER BY users.user_id\n      ",
    );
  let wrapped = ((), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((user_id, (name, (twoot_id, (content, likes))))) => (
        {name, user_id},
        {likes, content, twoot_id},
      );
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, ()), ~f);
  };
  wrapped;
};
let get_single_function_out = loaders => {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"] Caqti_type.(tup2(int, string)),
      "\n      SELECT id, name\n      FROM users\n      ",
    );
  let wrapped = (loader, (), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g = ((id, name)) => loader(~name, ~id);
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, ()), ~f);
  };
  wrapped(loaders);
};
let get_multiple_function_out = loaders => {
  let query =
    Caqti_request.(collect)(
      [@ocaml.warning "-33"] Caqti_type.(unit),
      [@ocaml.warning "-33"]
      Caqti_type.(tup2(int, tup2(string, tup2(int, tup2(string, int))))),
      "\n      SELECT users.id, users.name,\n             twoots.id, twoots.content, twoots.likes\n      FROM users\n      JOIN twoots ON twoots.id = users.id\n      ORDER BY users.id\n      ",
    );
  let wrapped = ((loader, loader'), (), module Db: Caqti_async.CONNECTION) => {
    let f = result => {
      let g =
          (
            (
              users_id,
              (users_name, (twoots_id, (twoots_content, twoots_likes))),
            ),
          ) => (
        loader(~id=users_id, ~name=users_name),
        loader'(~id=twoots_id, ~content=twoots_content, ~likes=twoots_likes),
      );
      let f = Stdlib.List.map(g);
      switch (result) {
      | Ok(x) => Ok(f(x))
      | Error(e) => Error(e)
      };
    };
    Async_kernel.Deferred.map(Db.collect_list(query, ()), ~f);
  };
  wrapped(loaders);
};

/* Simple queries */
type a = {username: string};

type b = {
  id: int,
  username: string,
};

let many_arg_execute = [%rapper
  execute(
    {sql|
      UPDATE users
      SET (username, email, bio) = (%string{username}, %string{email}, %string?{bio})
      WHERE id = %int{id}
      |sql},
  )
];

let many_arg_get_one_repeated_arg = [%rapper
  get_one(
    {sql|
      SELECT @string{username}
      FROM users
      WHERE id = %int{id} OR username = %string{username} OR id <> %int{id}
      |sql},
    record_out,
  )
];

let many_arg_get_opt = [%rapper
  get_opt(
    {sql|
      SELECT @int{id}, @string{username}
      FROM users
      WHERE username = %string{username} AND id > %int{min_id}
      |sql},
  )
];

/* Using list parameters */
type list_in = {versions: list(int)};

let collect_list = [%rapper
  get_many(
    {sql| SELECT @string{id} from schema_migrations where version in (%list{%int{versions}})|sql},
    record_in,
  )
];

/* Using custom types */
module Suit: Ppx_rapper_runtime.CUSTOM = {
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

let get_cards = [%rapper
  get_many(
    {sql| SELECT @int{id}, @Suit{suit} FROM cards WHERE suit <> %Suit{suit} |sql},
  )
];

/* Example showing the correspondence between rapper/Caqti types and OCaml types */
type all_types_output = {
  id: string,
  payload: string,
  version: int,
  some_int32: int32,
  some_int64: int64,
  added: bool,
  fl: float,
  date: Ptime.t,
  time: Ptime.t,
  span: Ptime.span,
};

let all_types = [%rapper
  get_many(
    {sql| SELECT @string{id}, @octets{payload}, @int{version},
                @int32{some_int32}, @int64{some_int64}, @bool{added},
                @float{fl}, @pdate{date}, @ptime{time}, @ptime_span{span}
         FROM some_table |sql},
    record_out,
  )
];

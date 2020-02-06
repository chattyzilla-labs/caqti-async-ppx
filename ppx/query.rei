/* From https://github.com/issuu/ppx_mysql
 * Under Apache 2.0 license */
/** {1 Type definitions} */;

type param = {
  typ: (option(string), string),
  opt: bool,
  name: string,
};

type list_params = {
  subsql: string,
  string_index: int,
  param_index: int,
  params: list(param),
};

type parsed_query = {
  sql: string,
  in_params: list(param),
  out_params: list(param),
  list_params: option(list_params),
};

type parse_error = [
  | `Bad_identifier(string)
  | `Unknown_type_spec(string)
  | `Empty_list_params
  | `Multiple_lists_not_supported
  | `Nested_list
  | `Optional_list
  | `Out_params_in_list
  | `Unterminated_list
  | `Unterminated_string
  | `Unterminated_bracket
  | `Escape_at_end
];

type conflict_error = [ | `Conflicting_spec(string)];

type error = [ parse_error | conflict_error];

/** {1 Public functions and values} */;

let parse: string => result(parsed_query, [> parse_error]);

let remove_duplicates:
  list(param) => result(list(param), [> conflict_error]);

let explain_error: [< error] => string;

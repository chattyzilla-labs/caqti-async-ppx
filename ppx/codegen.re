open Base;
open Ppxlib;
module Buildef = Ast_builder.Default;

type extension_contents = {
  in_params: list(Query.param),
  out_params: list(Query.param),
  record_in: bool,
  record_out: bool,
};

exception Error(string);

/** [up_to_last (xs @ [x])] returns [xs] */

let up_to_last = xs => List.take(xs, List.length(xs) - 1);

/** Produces individual Caqti types from parsed parameters */

/** Produces individual Caqti types from parsed parameters */

let caqti_type_of_param = (~loc, Query.{typ, opt, _}) => {
  let base_expr =
    switch (typ) {
    | (None, base_type) =>
      switch (base_type) {
      | "string" =>
        %expr
        string
      | "octets" =>
        %expr
        octets
      | "int" =>
        %expr
        int
      | "int32" =>
        %expr
        int32
      | "int64" =>
        %expr
        int64
      | "bool" =>
        %expr
        bool
      | "float" =>
        %expr
        float
      | "pdate" =>
        %expr
        pdate
      | "ptime" =>
        %expr
        ptime
      | "ptime_span" =>
        %expr
        ptime_span
      | other =>
        raise(Error(Printf.sprintf("Base type '%s' not supported", other)))
      }
    | (Some(module_name), typ) =>
      Buildef.pexp_ident(
        ~loc,
        Loc.make(~loc, [@implicit_arity] Ldot(Lident(module_name), typ)),
      )
    };

  opt
    ? Buildef.(pexp_apply(~loc, [%expr option], [(Nolabel, base_expr)]))
    : base_expr;
};

/** Makes Caqti type specifications like [string & option int & bool] */

let make_caqti_type_tup = (~loc, params) =>
  switch (List.length(params)) {
  | 0 =>
    %expr
    unit
  | _ =>
    let type_exprs = List.map(~f=caqti_type_of_param(~loc), params);
    let f = (elem_type_expr, apply_expr) => [%expr
      tup2([%e elem_type_expr], [%e apply_expr])
    ];

    List.fold_right(
      ~f,
      ~init=List.last_exn(type_exprs),
      up_to_last(type_exprs),
    );
  };

let lident_of_param = (~loc, param) =>
  Loc.make(~loc, Lident(param.Query.name));

let var_of_param = (~loc, param) => Loc.make(~loc, param.Query.name);

/** Maps parsed parameters to ident expressions of their names */

let pexp_idents_of_params = (~loc, params) =>
  List.map(
    ~f=param => Buildef.pexp_ident(~loc, lident_of_param(~loc, param)),
    params,
  );

let ppat_of_param = (~loc, param) =>
  Buildef.ppat_var(~loc, var_of_param(~loc, param));

/** Maps parsed parameters to var patterns of their names */

let ppat_var_of_params = (~loc, params) =>
  List.map(~f=ppat_of_param(~loc), params);

/** General function for producing ASTs for [(a, (b, (c, (d, e))))] as either expressions or patterns */

let nested_tuple_thing = (zero_case, mapper, tuple_maker, ~loc, params) =>
  switch (List.length(params)) {
  /* With current design, 0-tuple case should not occur. */
  | 0 => zero_case
  | _ =>
    let idents = mapper(~loc, params);
    let f = (ident, accum) => tuple_maker(~loc, [ident, accum]);
    List.fold_right(~f, ~init=List.last_exn(idents), up_to_last(idents));
  };

/** Makes [(a, (b, (c, (d, e))))] expression ASTs from parsed parameters */

let nested_tuple_expression = (~loc) =>
  nested_tuple_thing(
    [%expr ()],
    pexp_idents_of_params,
    Buildef.pexp_tuple,
    ~loc,
  );

/** Makes [(a, (b, (c, (d, e))))] pattern ASTs from parsed parameters */

let nested_tuple_pattern = (~loc) =>
  nested_tuple_thing(
    Buildef.ppat_tuple(~loc, []),
    ppat_var_of_params,
    Buildef.ppat_tuple,
    ~loc,
  );

/** Makes [(a, b, c, d, e)] expression ASTs from parsed parameters */

let flat_tuple = (~loc, params) =>
  Buildef.pexp_tuple(~loc, pexp_idents_of_params(~loc, params));

/** Makes [{a; b; c; d; e}] expression ASTs from parsed parameters */

let record_expression = (~loc, params) => {
  let f = param => {
    let lident = lident_of_param(~loc, param);
    (lident, Buildef.pexp_ident(~loc, lident));
  };

  let pair_list = List.map(params, ~f);
  Buildef.pexp_record(~loc, pair_list, None);
};

/** Makes [{a; b; c; d; e}] pattern ASTs from parsed parameters */

let record_pattern = (~loc, params) => {
  let f = param => {
    let lident = lident_of_param(~loc, param);
    let var = var_of_param(~loc, param);
    (lident, Buildef.ppat_var(~loc, var));
  };

  let pair_list = List.map(params, ~f);
  Buildef.ppat_record(~loc, pair_list, Closed);
};

/** Generates the function body for an [exec] function ([execute] statement) */

let function_body_exec =
    (~loc, connection_function_expr, {in_params, record_out, _}) => {
  assert(!record_out);
  let input_nested_tuples = nested_tuple_expression(~loc, in_params);
  %expr
  [%e connection_function_expr](query, [%e input_nested_tuples]);
};

let function_body_general =
    (
      ~loc,
      factory,
      connection_function_expr,
      {in_params, out_params, record_out, _},
    ) => {
  let input_nested_tuple_expression =
    nested_tuple_expression(~loc, in_params);
  switch (List.length(out_params), record_out) {
  | (0, true) =>
    raise(
      Error(
        "'record_out' should not be set when there are no output parameters",
      ),
    )
  | (0, false)
  | (1, false) =>
    %expr
    [%e connection_function_expr](query, [%e input_nested_tuple_expression])
  | (1, true)
  | _ =>
    let input_nested_tuple_pattern = nested_tuple_pattern(~loc, out_params);
    let output_expression =
      if (record_out) {
        record_expression(~loc, out_params);
      } else {
        flat_tuple(~loc, out_params);
      };

    factory(
      ~loc,
      input_nested_tuple_pattern,
      output_expression,
      connection_function_expr,
      input_nested_tuple_expression,
    );
  };
};

let find_body_factory =
    (
      ~loc,
      input_nested_tuple_pattern,
      output_expression,
      connection_function_expr,
      input_nested_tuple_expression,
    ) => {
  let%expr f = result =>
    switch (result) {
    | Ok([%p input_nested_tuple_pattern]) => Ok([%e output_expression])
    | Error(e) => Error(e)
    };
    open Async_kernel;
  [%e connection_function_expr](query, [%e input_nested_tuple_expression]) >>| f;

};

let find_map_factory =
    (
      ~loc,
      map_expr,
      input_nested_tuple_pattern,
      output_expression,
      connection_function_expr,
      input_nested_tuple_expression,
    ) => {
  let%expr f = result => {
    let g = ([%p input_nested_tuple_pattern]) => [%e output_expression];
    let f = [%e map_expr](g);
    switch (result) {
    | Ok(x) => Ok(f(x))
    | Error(e) => Error(e)
    };
  };
    open Async_kernel;
    [%e connection_function_expr](query, [%e input_nested_tuple_expression]) >>| f;
};

/** Generates the function body for a [find] function ([get_one] statement)*/

let function_body_find = (~loc) =>
  function_body_general(~loc, find_body_factory);

/** Generates the function body for cases where it has involves a map
 * These are [find_opt] and [collect_list] (for [get_opt] and [get_many] statements). */

let function_body_map = (~loc, map_expr) =>
  function_body_general(~loc, find_map_factory(map_expr));

/** Generates the function body for a [find_opt] function ([get_opt] statement) */

let function_body_find_opt = (~loc) =>
  function_body_map(
    ~loc,
    [%expr
      (f, x) =>
        switch (x) {
        | Some(x) => Some(f(x))
        | None => None
        }
    ],
  );

/** Generates the function body for a [collect_list] function ([get_many] statement) */

let function_body_collect = (~loc) =>
  function_body_map(~loc, [%expr Stdlib.List.map]);

/** Generates code like [fun ~x ~y ~z -> Db.some_function query (x, (y, z))]. */

let query_function =
    (
      ~loc,
      ~body_fn=x => x,
      function_body_factory,
      connection_function_expr,
      expression_contents,
    ) => {
  /* Tuples should have duplicates if they exist. */
  let body =
    function_body_factory(~loc, connection_function_expr, expression_contents)
    |> body_fn;

  let in_params = expression_contents.in_params;
  let deduped_in_params =
    switch (Query.remove_duplicates(in_params)) {
    | Ok(deduplicated) => deduplicated
    | Error(_) =>
      raise(Error("Duplicated input parameters with conflicting specs"))
    };

  expression_contents.record_in
    ? if (List.is_empty(in_params)) {
        raise(
          Error(
            "'record_in' should not be set when there are no input parameters",
          ),
        );
      } else {
        let input_record_pattern = record_pattern(~loc, deduped_in_params);
        %expr
        ([%p input_record_pattern]) => [%e body];
      }
    : (
      if (List.is_empty(in_params)) {
        %expr
        () => [%e body];
      } else {
        let f = (in_param, body_so_far) => {
          let name = in_param.Query.name;
          let pattern = Buildef.ppat_var(~loc, Loc.make(~loc, name));
          Buildef.pexp_fun(~loc, Labelled(name), None, pattern, body_so_far);
        };

        List.fold_right(~f, ~init=body, deduped_in_params);
      }
    );
};
let exec_function = (~body_fn, ~loc) =>
  query_function(~loc, ~body_fn, function_body_exec, [%expr Db.exec]);
let find_function = (~body_fn, ~loc) =>
  query_function(~loc, ~body_fn, function_body_find, [%expr Db.find]);
let find_opt_function = (~body_fn, ~loc) =>
  query_function(~loc, ~body_fn, function_body_find_opt, [%expr Db.find_opt]);
let collect_list_function = (~body_fn, ~loc) =>
  query_function(
    ~loc,
    ~body_fn,
    function_body_collect,
    [%expr Db.collect_list],
  );
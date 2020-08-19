open Base;
open Ppxlib;
module Buildef = Ast_builder.Default;

type input_kind = [ | `Labelled_args | `Record];

type output_kind = [ | `Tuple | `Record | `Function];

type extension_contents = {
  in_params: list(Query.param),
  out_params: list(Query.param),
  input_kind,
  output_kind,
};

type record_output_form =
  | One(list(Query.param))
  | Many(list(list(Query.param)));

exception Error(string);

/** [up_to_last (xs @ [x])] returns [xs] */

let up_to_last = xs => List.take(xs, List.length(xs) - 1);

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
      /* This case covers [cdate] and [ctime] */
      Buildef.pexp_ident(
        ~loc,
        Loc.make(~loc, Longident.parse(module_name ++ "." ++ typ)),
      )
    };

  opt
    ? Buildef.(pexp_apply(~loc, [%expr option], [(Nolabel, base_expr)]))
    : base_expr;
};

let caqti_type_tup_of_expressions = (~loc, expressions) =>
  switch (List.length(expressions)) {
  | 0 =>
    %expr
    unit
  | _ =>
    let f = (elem_type_expr, apply_expr) => [%expr
      tup2([%e elem_type_expr], [%e apply_expr])
    ];

    List.fold_right(
      ~f,
      ~init=List.last_exn(expressions),
      up_to_last(expressions),
    );
  };

/** Makes Caqti type specifications like [string & option int & bool] */

let make_caqti_type_tup = (~loc, params) =>
  switch (List.length(params)) {
  | 0 =>
    %expr
    unit
  | _ =>
    let type_exprs = List.map(~f=caqti_type_of_param(~loc), params);
    caqti_type_tup_of_expressions(~loc, type_exprs);
  };

let lident_of_string = (~loc, s) => Loc.make(~loc, Lident(s));

let nth_loader_string = n =>
  "loader" ++ (List.init(n, ~f=_ => "'") |> String.concat);

let nth_loader = (~loc, n) => {
  let s = nth_loader_string(n);
  Buildef.(pexp_ident(~loc, lident_of_string(~loc, s)));
};

let nth_loader_pat = (~loc, n) => {
  let s = nth_loader_string(n);
  Buildef.(ppat_var(~loc, Loc.make(~loc, s)));
};

let lident_of_param = (~loc, param) =>
  lident_of_string(~loc, param.Query.name);

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

/** Makes [[x; y; z]] expression ASTs from a list of expressions */

let list_expression = (~loc, xs) =>
  List.fold_right(
    ~init=[%expr []],
    ~f=
      (x, accum) =>
        [%expr [[%e x], ...[%e accum]]],
    xs,
  );

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
    (~loc, connection_function_expr, {in_params, output_kind, _}) => {
  assert(!Poly.(output_kind == `Record));
  let input_nested_tuples = nested_tuple_expression(~loc, in_params);
  %expr
  [%e connection_function_expr](query, [%e input_nested_tuples]);
};

let output_field_name = qualified_param_name => {
  let split = String.split(qualified_param_name, ~on='.');
  List.last_exn(split);
};

let pattern_field_name = qualified_param_name =>
  String.map(
    qualified_param_name,
    ~f=
      fun
      | '.' => '_'
      | other => other,
  );

let make_qualified_output = out_params =>
  if (List.for_all(out_params, ~f=({Query.name, _}) =>
        !String.contains(name, '.')
      )) {
    One(out_params);
  } else {
    let record_name_groups = Hashtbl.create((module String));
    let sorted_keys = ref([]);
    List.iter(
      out_params,
      ~f=param => {
        let split = String.split(param.Query.name, ~on='.');
        let key = up_to_last(split) |> String.concat(~sep="_");
        if (!Hashtbl.mem(record_name_groups, key)) {
          sorted_keys := sorted_keys^ @ [key];
        };
        let current =
          Hashtbl.find_or_add(record_name_groups, key, ~default=() => []);

        Hashtbl.set(record_name_groups, ~key, ~data=[param, ...current]);
      },
    );
    let sorted_groups =
      List.map(sorted_keys^, ~f=Hashtbl.find_exn(record_name_groups));

    Many(sorted_groups);
  };

let with_output_field_names = out_params =>
  List.map(out_params, ~f=param =>
    {...param, Query.name: output_field_name(param.Query.name)}
  );

let with_pattern_field_names = out_params =>
  List.map(out_params, ~f=param =>
    {...param, Query.name: pattern_field_name(param.Query.name)}
  );

/** Generates the function body for statements that aren't executes */

let function_body_general =
    (
      ~loc,
      factory,
      connection_function_expr,
      {in_params, out_params, output_kind, _},
    ) => {
  let input_nested_tuple_expression =
    nested_tuple_expression(~loc, in_params);
  switch (List.length(out_params), output_kind) {
  | (0, `Record) =>
    raise(
      Error(
        "'record_out' should not be set when there are no output parameters",
      ),
    )
  | (0, `Tuple)
  | (1, `Tuple) =>
    %expr
    [%e connection_function_expr](query, [%e input_nested_tuple_expression])
  | (_, `Record) =>
    let (output_nested_tuple_pattern, output_expression) =
      switch (make_qualified_output(out_params)) {
      | One(out_params) => (
          nested_tuple_pattern(~loc, out_params),
          record_expression(~loc, out_params),
        )
      | Many(out_params_groups) => (
          nested_tuple_pattern(~loc, with_output_field_names(out_params)),
          Buildef.pexp_tuple(
            ~loc,
            List.map(out_params_groups, ~f=ps =>
              record_expression(~loc, with_output_field_names(ps))
            ),
          ),
        )
      };

    factory(
      ~loc,
      output_nested_tuple_pattern,
      output_expression,
      connection_function_expr,
      input_nested_tuple_expression,
    );
  | (_, `Tuple) =>
    let (output_nested_tuple_pattern, output_expression) = (
      nested_tuple_pattern(~loc, out_params),
      flat_tuple(~loc, out_params),
    );

    factory(
      ~loc,
      output_nested_tuple_pattern,
      output_expression,
      connection_function_expr,
      input_nested_tuple_expression,
    );
  | (_, `Function) =>
    let arg_list_of_params =
      List.map(~f=({Query.name, _}) => {
        let label = output_field_name(name);
        let argument = pattern_field_name(name);
        Buildef.(
          Labelled(label),
          pexp_ident(~loc, lident_of_string(~loc, argument)),
        );
      });

    let function_expression = (~loc, n, out_params) =>
      Buildef.(
        pexp_apply(
          ~loc,
          nth_loader(~loc, n),
          arg_list_of_params(List.rev(out_params)),
        )
      );

    let (output_nested_tuple_pattern, output_expression) =
      switch (make_qualified_output(out_params)) {
      | One(out_params) => (
          nested_tuple_pattern(~loc, out_params),
          function_expression(~loc, 0, out_params),
        )
      | Many(out_params_groups) => (
          nested_tuple_pattern(~loc, with_pattern_field_names(out_params)),
          Buildef.pexp_tuple(
            ~loc,
            List.mapi(out_params_groups, ~f=function_expression(~loc)),
          ),
        )
      };

    factory(
      ~loc,
      output_nested_tuple_pattern,
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
    Async_kernel.Deferred.map(
      [%e connection_function_expr](query, [%e input_nested_tuple_expression]),
      ~f
    );
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
  Async_kernel.Deferred.map(
    [%e connection_function_expr](query, [%e input_nested_tuple_expression]),
    ~f,
  );
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
  let without_loaders_parameter = {
    /* Tuples should have duplicates if they exist. */
    let body =
      function_body_factory(
        ~loc,
        connection_function_expr,
        expression_contents,
      )
      |> body_fn;

    let in_params = expression_contents.in_params;
    let deduped_in_params =
      switch (Query.remove_duplicates(in_params)) {
      | Ok(deduplicated) => deduplicated
      | Error(_) =>
        raise(Error("Duplicated input parameters with conflicting specs"))
      };

    switch (expression_contents.input_kind) {
    | `Record =>
      if (List.is_empty(in_params)) {
        raise(
          Error(
            "'record_in' should not be set when there are no input parameters",
          ),
        );
      } else {
        let input_record_pattern = record_pattern(~loc, deduped_in_params);
        %expr
        (
          ([%p input_record_pattern], module Db: Caqti_async.CONNECTION) => [%e
            body
          ]
        );
      }
    | `Labelled_args =>
      if (List.is_empty(in_params)) {
        %expr
        (((), module Db: Caqti_async.CONNECTION) => [%e body]);
      } else {
        let f = (in_param, body_so_far) => {
          let name = in_param.Query.name;
          let pattern = Buildef.ppat_var(~loc, Loc.make(~loc, name));
          Buildef.pexp_fun(~loc, Labelled(name), None, pattern, body_so_far);
        };

        List.fold_right(
          ~f,
          ~init=[%expr (module Db: Caqti_async.CONNECTION) => [%e body]],
          deduped_in_params,
        );
      }
    };
  };

  switch (expression_contents.output_kind) {
  | `Function =>
    let qualified_out_params =
      make_qualified_output(expression_contents.out_params);

    switch (qualified_out_params) {
    | One(_) =>
      %expr
      (loader => [%e without_loaders_parameter])
    | Many(groups) =>
      let loaders =
        Buildef.ppat_tuple(
          ~loc,
          List.init(List.length(groups), ~f=n => nth_loader_pat(~loc, n)),
        );

      %expr
      (([%p loaders]) => [%e without_loaders_parameter]);
    };
  | _ => without_loaders_parameter
  };
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

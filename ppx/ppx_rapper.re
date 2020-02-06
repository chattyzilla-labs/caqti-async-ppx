open Base;
open Ppxlib;
module Buildef = Ast_builder.Default;

/** Handle 'record_in' etc. in [%rapper "SELECT * FROM USERS" record_in record_out] */

let parse_args = args => {
  let allowed_args = ["record_in", "record_out", "syntax_off"];
  switch (
    List.find(~f=a => !List.mem(~equal=String.equal, allowed_args, a), args)
  ) {
  | Some(unknown) =>
    Error(Printf.sprintf("Unknown rapper argument '%s'", unknown))
  | None =>
    let record_in = List.mem(args, "record_in", ~equal=String.equal);
    let record_out = List.mem(args, "record_out", ~equal=String.equal);
    let syntax_off = List.mem(args, "syntax_off", ~equal=String.equal);
    [@implicit_arity] Ok(record_in, record_out, syntax_off);
  };
};

/** Make some subexpressions to be used in generated code */

let component_expressions = (~loc, parsed_query) => {
  open Query;
  let inputs_caqti_type =
    Codegen.make_caqti_type_tup(~loc, parsed_query.in_params);

  let outputs_caqti_type =
    Codegen.make_caqti_type_tup(~loc, parsed_query.out_params);

  let parsed_sql = Buildef.estring(~loc, parsed_query.sql);
  (inputs_caqti_type, outputs_caqti_type, parsed_sql);
};

/** Make a function [expand_get] to produce the expressions for [get_one], [get_opt] and [get_many], and a similar [expand_exec] for [execute] */

let make_expand_get_and_exec_expression =
    (~loc, parsed_query, record_in, record_out) => {
  let {Query.sql, in_params, out_params, list_params} = parsed_query;
  switch (list_params) {
  | Some({subsql, string_index, param_index, params}) =>
    assert(List.length(params) == 1);
    let subsql_expr = Buildef.estring(~loc, subsql);
    let sql_before =
      Buildef.estring(~loc) @@ String.sub(sql, ~pos=0, ~len=string_index);
    let sql_after =
      Buildef.estring(~loc) @@
      String.sub(
        sql,
        ~pos=string_index,
        ~len=String.length(sql) - string_index,
      );

    let (params_before, params_after) = List.split_n(in_params, param_index);
    let expression_contents = {
      Codegen.in_params: params_before @ params @ params_after,
      out_params,
      record_in,
      record_out,
    };

    let caqti_input_type =
      switch (List.is_empty(params_before), List.is_empty(params_after)) {
      | (true, true) =>
        %expr
        packed_list_type
      | (true, false) =>
        let params_before = Codegen.make_caqti_type_tup(~loc, params_before);
        %expr
        Caqti_type.(tup2([%e params_before], packed_list_type));
      | (false, true) =>
        let params_before = Codegen.make_caqti_type_tup(~loc, params_before);
        %expr
        Caqti_type.(tup2([%e params_before], packed_list_type));
      | (false, false) =>
        let params_before = Codegen.make_caqti_type_tup(~loc, params_before);
        let params_after = Codegen.make_caqti_type_tup(~loc, params_after);
        %expr
        Caqti_type.(
          tup3([%e params_before], packed_list_type, [%e params_after])
        );
      };

    let outputs_caqti_type = Codegen.make_caqti_type_tup(~loc, out_params);
    let list_param = List.hd_exn(params);
    let make_generic = (make_function, query_expr) => {
      let body_fn = body =>
        switch%expr (
          [%e
            Buildef.pexp_ident(
              ~loc,
              Codegen.lident_of_param(~loc, list_param),
            )
          ]
        ) {
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
          let subsqls = Stdlib.List.map(_ => [%e subsql_expr], elems);
          let patch = Stdlib.String.concat(", ", subsqls);
          let sql = [%e sql_before] ++ patch ++ [%e sql_after];
          open Ppx_rapper_runtime;
          let [@implicit_arity]
              Dynparam.Pack(
                packed_list_type,
                [%p Codegen.ppat_of_param(~loc, list_param)],
              ) =
            Stdlib.List.fold_left(
              (pack, item) =>
                Dynparam.add(
                  {
                    open Caqti_type;
                    %e
                    Codegen.make_caqti_type_tup(~loc, [list_param]);
                  },
                  item,
                  pack,
                ),
              Dynparam.empty,
              elems,
            );

          let query = [%e query_expr];
          %e
          body;
        };

      let%expr wrapped = (module Db: Caqti_async.CONNECTION) => [%e
        make_function(~body_fn, ~loc, expression_contents)
      ];

      wrapped;
    };

    let expand_get = (caqti_request_function_expr, make_function) =>
      try (
        Ok(
          make_generic(
            make_function,
            [%expr
              {
                open Caqti_request;
                %e
                caqti_request_function_expr;
              }(
                ~oneshot=true,
                [@ocaml.warning "-33"] [%e caqti_input_type],
                [@ocaml.warning "-33"]
                {
                  open Caqti_type;
                  %e
                  outputs_caqti_type;
                },
                sql,
              )
            ],
          ),
        )
      ) {
      | Codegen.Error(s) => Error(s)
      };

    let expand_exec = (caqti_request_function_expr, make_function) =>
      try (
        Ok(
          make_generic(
            make_function,
            [%expr
              {
                open Caqti_request;
                %e
                caqti_request_function_expr;
              }(
                [%e caqti_input_type],
                sql,
              )
            ],
          ),
        )
      ) {
      | Codegen.Error(s) => Error(s)
      };

    (expand_get, expand_exec);

  | None =>
    let (inputs_caqti_type, outputs_caqti_type, parsed_sql) =
      component_expressions(~loc, parsed_query);

    let expression_contents =
      Codegen.{
        in_params: parsed_query.in_params,
        out_params: parsed_query.out_params,
        record_in,
        record_out,
      };

    let make_generic = (make_function, query_expr) => {
      let%expr query = [%e query_expr];
      let wrapped = (module Db: Caqti_async.CONNECTION) => [%e
        make_function(~body_fn=x => x, ~loc, expression_contents)
      ];

      wrapped;
    };

    let expand_get = (caqti_request_function_expr, make_function) =>
      try (
        Ok(
          make_generic(
            make_function,
            [%expr
              {
                open Caqti_request;
                %e
                caqti_request_function_expr;
              }(
                [@ocaml.warning "-33"]
                {
                  open Caqti_type;
                  %e
                  inputs_caqti_type;
                },
                [@ocaml.warning "-33"]
                {
                  open Caqti_type;
                  %e
                  outputs_caqti_type;
                },
                [%e parsed_sql],
              )
            ],
          ),
        )
      ) {
      | Codegen.Error(s) => Error(s)
      };

    let expand_exec = (caqti_request_function_expr, make_function) =>
      try (
        Ok(
          make_generic(
            make_function,
            [%expr
              {
                open Caqti_request;
                %e
                caqti_request_function_expr;
              }(
                [@ocaml.warning "-33"]
                {
                  open Caqti_type;
                  %e
                  inputs_caqti_type;
                },
                [%e parsed_sql],
              )
            ],
          ),
        )
      ) {
      | Codegen.Error(s) => Error(s)
      };

    (expand_get, expand_exec);
  };
};

let expand = (~loc, ~path as _, action, query, args) => {
  let expression_result =
    switch (parse_args(args)) {
    | Error(err) => Error(err)
    | [@implicit_arity] Ok(record_in, record_out, syntax_off) =>
      switch (Query.parse(query)) {
      | Error(error) => Error(Query.explain_error(error))
      | Ok(parsed_query) =>
        let syntax_result =
          switch (syntax_off) {
          | false =>
            let query_sql =
              switch (parsed_query.list_params) {
              | Some({subsql, string_index, _}) =>
                let sql = parsed_query.sql;
                let sql_before = String.sub(sql, ~pos=0, ~len=string_index);
                let sql_after =
                  String.sub(
                    sql,
                    ~pos=string_index,
                    ~len=String.length(sql) - string_index,
                  );

                sql_before ++ subsql ++ sql_after;
              | None => parsed_query.sql
              };

            switch (Pg_query.parse(query_sql)) {
            | Ok(_) => Ok()
            | Error(msg) =>
              Error(Printf.sprintf("Syntax error in SQL: '%s'", msg))
            };
          | true => Ok()
          };

        switch (syntax_result) {
        | Error(msg) => Error(msg)
        | Ok () =>
          Ok(
            {
              let (expand_get, expand_exec) =
                make_expand_get_and_exec_expression(
                  ~loc,
                  parsed_query,
                  record_in,
                  record_out,
                );

              switch (action) {
              /* execute is special case because there is no output Caqti_type */
              | "execute" =>
                if (record_out) {
                  Error("record_out is not a valid argument for execute");
                } else {
                  expand_exec([%expr exec], Codegen.exec_function);
                }
              | "get_one" => expand_get([%expr find], Codegen.find_function)
              | "get_opt" =>
                expand_get([%expr find_opt], Codegen.find_opt_function)
              | "get_many" =>
                expand_get([%expr collect], Codegen.collect_list_function)
              | _ =>
                Error(
                  "Supported actions are execute, get_one, get_opt and get_many",
                )
              };
            },
          )
        };
      }
    };

  switch (expression_result) {
  | Ok(Ok(expr)) => expr
  | Ok(Error(msg))
  | Error(msg) =>
    raise(
      Location.Error(
        Location.Error.createf(~loc, "Error in ppx_rapper: %s", msg),
      ),
    )
  };
};

/** Captures [[%rapper get_one "SELECT id FROM things WHERE condition"]] */

let pattern = {
  open Ast_pattern;
  let query_action = pexp_ident(lident(__));
  let query = pair(nolabel, estring(__));
  let arg = pair(nolabel, pexp_ident(lident(__)));
  /*   let arg_opt = alt_option (arg ^:: nil) nil in */
  /*   let arg2 = pair nolabel (pexp_ident (lident __)) in */
  /*   let arg2_opt = alt_option (arg2 ^:: nil) nil in */
  let arguments = query ^:: many(arg);
  pexp_apply(query_action, arguments);
};

let name = "rapper";

let ext =
  Extension.declare(
    name,
    Extension.Context.expression,
    Ast_pattern.(single_expr_payload(pattern)),
    expand,
  );

let () = Driver.register_transformation(name, ~extensions=[ext]);

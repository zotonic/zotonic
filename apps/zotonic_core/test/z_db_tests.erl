%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(z_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

alter_table_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_db:drop_table(dbtests, Context),
    A = [
        #column_def{
            name = id, type = "serial", is_nullable = false, primary_key = true
        },
        #column_def{
            name = a, type = "character varying", length = 20
        },
        #column_def{
            name = d, type = "integer", length = undefined
        },
        #column_def{
            name = e, type = "float", length = undefined
        }
    ],
    ok = z_db:alter_table(dbtests, A, Context),
    {ok, #column_def{ name = id, type = <<"serial">> }} = z_db:column(dbtests, id, Context),
    {ok, #column_def{ name = a, type = <<"character varying">>, length = 20 }} = z_db:column(dbtests, a, Context),
    {ok, #column_def{ name = d, type = <<"integer">> }} = z_db:column(dbtests, d, Context),
    {error, enoent} = z_db:column(dbtests, f, Context),
    A1 = [
        #column_def{
            name = id, type = <<"bigserial">>, is_nullable = false, primary_key = true
        },
        #column_def{
            name = a, type = "character varying", length = 10
        },
        #column_def{
            name = d, type = "varchar", length = 30
        },
        #column_def{
            name = f, type = "integer", length = undefined
        }
    ],
    ok = z_db:alter_table(dbtests, A1, Context),
    {ok, #column_def{ name = id, type = <<"bigserial">> }} = z_db:column(dbtests, id, Context),
    {ok, #column_def{ name = a, type = <<"character varying">>, length = 10 }} = z_db:column(dbtests, a, Context),
    {ok, #column_def{ name = d, type = <<"character varying">>, length = 30 }} = z_db:column(dbtests, d, Context),
    {ok, #column_def{ name = f, type = <<"integer">> }} = z_db:column(dbtests, f, Context),
    {error, enoent} = z_db:column(dbtests, e, Context),
    ok = z_db:drop_table(dbtests, Context),
    ok.

postgres_datetime_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    ?assertMatch( [{{{_,_,_},{_,_,S}}}] when is_integer(S), z_db:q("select now();", Context)),
    ?assertMatch( [{[{{_,_,_},{_,_,S}}]}] when is_integer(S), z_db:q("select array_agg(now());", Context)),
    ?assertMatch( [{[{{{_,_,_},{_,_, S}}, _} ]}] when is_integer(S), z_db:q("select array_agg(row(now(), 1));", Context)),

    ok.

postgres_null_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    [{undefined}] = z_db:q("select null;", Context),
    ok.

postgres_eterm_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    %% Should be encoded as null value in postgres, which is returned as undefined.
    [{undefined}] = z_db:q("select $1::bytea;", [{term, undefined}], Context),

    [{#{ test := 123 }}] = z_db:q("select $1::bytea;", [{term, #{ test => 123 }}], Context),
    ok.

postgres_json_conversion_test() ->
    Context = z_context:new(zotonic_site_testsandbox),

    %% Should be encoded as null value in postgres, which is returned as undefined.
    [{undefined}] = z_db:q("select $1::jsonb;", [{term_json, undefined}], Context),

    %% Currently the result is not decoded.
    [{<<"{\"test\": 123}">>}] = z_db:q("select $1::jsonb;", [{term_json, #{ test => 123 }}], Context),

    %% Check if jsxrecord is used to encode the terms.
    [{<<"{\"list\": [1], \"_type\": \"rsc_list\"}">>}] =
        z_db:q("select $1::jsonb;", [{term_json, #rsc_list{ list = [1] }}], Context),

    ok.

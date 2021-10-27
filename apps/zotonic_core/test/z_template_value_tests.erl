%% @author Marc Worrell
%% @hidden

-module(z_template_value_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


value_lookup_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual(
        <<"administrator">>,
        z_template_compiler_runtime:find_nested_value([ <<"a">>, <<"atlas:post">>, <<"name">> ], vars(), Context)),
    ?assertEqual(
        <<"administrator">>,
        z_template_compiler_runtime:find_nested_value([ <<"a">>, <<"atlas:post">>, 1, <<"name">> ], vars(), Context)),
    ?assertEqual(
        undefined,
        z_template_compiler_runtime:find_nested_value([ <<"a">>, <<"atlas:post">>, 2, <<"name">> ], vars(), Context)),

    ?assertEqual(
        <<"administrator">>,
        z_template_compiler_runtime:find_nested_value([ <<"b">>, <<"atlas:post">>, <<"name">> ], vars(), Context)),
    ?assertEqual(
        <<"administrator">>,
        z_template_compiler_runtime:find_nested_value([ <<"b">>, <<"atlas:post">>, 1, <<"name">> ], vars(), Context)),
    ?assertEqual(
        undefined,
        z_template_compiler_runtime:find_nested_value([ <<"b">>, <<"atlas:post">>, 2, <<"name">> ], vars(), Context)),
    ok.

vars() ->
    #{
        <<"a">> => #{
            <<"atlas:post">> => #{ <<"@id">> => <<"/id/1">> }
        },

        <<"b">> => #{
            <<"atlas:post">> => [
                #{ <<"@id">> => <<"/id/1">> }
            ]
        }
    }.

-module(z_config_tests).

-include_lib("eunit/include/eunit.hrl").

yaml_nested_test() ->
    Out = #{
        a => [ 1, 2, 3 ],
        b => #{
            c => 1,
            d => [ 1, 2, 3 ],
            e => #{
                f => 1,
                g => 2
            }
        }
    },
    {ok, Out} = z_sites_config:read_configs([
            test_file("nested.yml")
        ]),
    ok.

yaml_overlay_test() ->
    Overlay = #{
        foo => <<"bar">>,
        a => <<"b">>,
        b => 2,
        c => 3
    },
    {ok, Overlay} = z_sites_config:read_configs([
            test_file("a.yml"),
            test_file("b.yml")
        ]),
    ok.

yaml_nofile_test() ->
    F = test_file("notfound.yml"),
    {error, {config_file, yaml_format, F, _}} = z_sites_config:read_configs([ F ]),
    ok.

json_overlay_test() ->
    Overlay = #{
        foo => <<"bar">>,
        a => <<"b">>,
        b => 2,
        c => 3
    },
    {ok, Overlay} = z_sites_config:read_configs([
            test_file("a.json"),
            test_file("b.json")
        ]),
    ok.

config_overlay_test() ->
    Overlay = #{
        foo => "bar",
        a => "b",
        b => 2,
        c => 3
    },
    {ok, Overlay} = z_sites_config:read_configs([
            test_file("a.config"),
            test_file("b.config")
        ]),
    ok.

test_file( Name ) ->
    filename:join([ code:lib_dir(zotonic_core), "test", "data", "config", Name ]).

%% @copyright 2026 Marc Worrell
%% @doc Database-free tests for the media runner client configuration contract.

-module(z_media_runner_config_tests).

-include_lib("eunit/include/eunit.hrl").

configuration_test_() ->
    Entry = #{hostname => <<"media.example.com">>, oauth2_key => <<"secret">>},
    Runner = #{url => <<"https://media.example.com/media-runner/jobs">>, token => <<"secret">>},
    Invalid = {error, media_runner_configuration},
    Cases = [
        {"unset", [], false, {ok, []}},
        {"empty hostname", [{media_runner_hostname, <<>>}], false, {ok, []}},
        {"empty string hostname", [{media_runner_hostname, ""}], false, {ok, []}},
        {"single runner", single("media.example.com", "secret"), true, {ok, [Runner]}},
        {"single HTTP runner", [{media_runner_protocol, "http"} | single("media.example.com", "secret")],
            true, {ok, [Runner#{url => <<"http://media.example.com/media-runner/jobs">>}]}},
        {"missing token", [{media_runner_hostname, "media.example.com"}], true, Invalid},
        {"invalid hostname type", single(false, "secret"), true, Invalid},
        {"invalid token type", single("media.example.com", undefined), true, Invalid},
        {"header injection", single("media.example.com", <<"secret\r\nx-extra: value">>), true, Invalid},
        {"trailing newline in token", single("media.example.com", <<"secret\n">>), true, Invalid},
        {"pool", [{media_runners, [Entry]}], true, {ok, [Runner]}},
        {"binary keys", [{media_runners, [#{<<"hostname">> => "media.example.com",
            <<"oauth2_key">> => "secret", <<"protocol">> => "https"}]}], true, {ok, [Runner]}},
        {"pool overrides invalid single", [{media_runners, [Entry]} | single(false, undefined)],
            true, {ok, [Runner]}},
        {"empty pool disables single", [{media_runners, []} | single("media.example.com", "secret")],
            false, {ok, []}},
        {"invalid pool never uses single", [{media_runners, false} | single("media.example.com", "secret")],
            true, Invalid},
        {"invalid entry rejects whole pool", [{media_runners, [Entry, #{}]}], true, Invalid},
        {"duplicate runner", [{media_runners, [Entry, Entry]}], true, Invalid},
        {"oversized pool", [{media_runners, lists:duplicate(33, Entry)}], true, Invalid},
        {"fallback option cannot hide error", [{media_runner_local_fallback, true} |
            single("media.example.com", <<>>)], true, Invalid},
        {"fallback option cannot change remote mode", [{media_runner_local_fallback, true} |
            single("media.example.com", "secret")], true, {ok, [Runner]}}
    ],
    [{Name, fun() -> with_config(Config, fun() ->
        ?assertEqual(Configured, z_media_runner_pool:configured()),
        ?assertEqual(Expected, z_media_runner_pool:runners())
    end) end} || {Name, Config, Configured, Expected} <- Cases].

single(Host, Token) ->
    [{media_runner_hostname, Host}, {media_runner_oauth2_key, Token}].

endpoint_test_() ->
    [
        ?_assertEqual({ok, <<"https://media.example.com/media-runner/jobs">>},
            z_media_runner_protocol:endpoint("media.example.com")),
        ?_assertEqual({ok, <<"http://localhost:8080/media-runner/jobs">>},
            z_media_runner_protocol:endpoint(<<"localhost:8080">>, "http")),
        ?_assertEqual({ok, <<"https://[::1]:8443/media-runner/jobs">>},
            z_media_runner_protocol:endpoint("[::1]:8443")),
        ?_assertEqual({error, media_runner_configuration},
            z_media_runner_protocol:endpoint("media.example.com", "ftp"))
    ] ++ [
        {lists:flatten(io_lib:format("invalid host ~p", [Host])), fun() ->
            ?assertEqual({error, media_runner_configuration}, z_media_runner_protocol:endpoint(Host))
        end} || Host <- [undefined, false, 42, <<>>, "https://media.example.com",
            "user:pass@media.example.com", "media.example.com/", "media.example.com/path",
            "media.example.com?x=1", "media.example.com#fragment", "media.example.com:0",
            "media.example.com:65536", "media.example.com:bad", "media.example.com:",
            "media.example.com\r\nx-host: other", lists:duplicate(254, $a)]
    ].

credential_scoped_identity_test() ->
    Runner = #{url => <<"https://media.example.com/media-runner/jobs">>, token => <<"first">>},
    ?assertNotEqual(z_media_runner_pool:identity(Runner),
        z_media_runner_pool:identity(Runner#{token => <<"second">>})),
    ?assertNotEqual(z_media_runner_pool:identity(Runner),
        z_media_runner_pool:identity(Runner#{url => <<"https://other.example.com/media-runner/jobs">>})).

http_policy_test_() ->
    [{atom_to_list(Environment), fun() ->
        with_config([{environment, Environment}], fun() ->
            Options = z_media_runner_protocol:http_options(120000),
            ?assertEqual(false, proplists:get_value(autoredirect, Options)),
            ?assertEqual([{verify, verify_none}], proplists:get_value(ssl, Options)),
            ?assertEqual(5000, proplists:get_value(connect_timeout, Options)),
            ?assertEqual(120000, proplists:get_value(timeout, Options))
        end)
    end} || Environment <- [development, test, production]].

%% Tests are sequential and restore the calling VM's application environment.
with_config(Config, Fun) ->
    Keys = [media_runners, media_runner_hostname, media_runner_protocol,
        media_runner_oauth2_key, media_runner_local_fallback, environment],
    Saved = [{Key, application:get_env(zotonic, Key)} || Key <- Keys],
    try
        lists:foreach(fun(Key) -> application:unset_env(zotonic, Key) end, Keys),
        lists:foreach(fun({Key, Value}) -> application:set_env(zotonic, Key, Value) end, Config),
        Fun()
    after
        lists:foreach(fun
            ({Key, undefined}) -> application:unset_env(zotonic, Key);
            ({Key, {ok, Value}}) -> application:set_env(zotonic, Key, Value)
        end, Saved)
    end.

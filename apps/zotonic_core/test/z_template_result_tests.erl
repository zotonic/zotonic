-module(z_template_result_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

render_result_test_() ->
    %% Core mocks must not affect background testsandbox workers.
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1,
        [fun failed_render/0, fun empty_render/0, fun successful_render/0]}).

setup() ->
    Modules = [template_compiler, z_notifier, m_config],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    meck:expect(z_notifier, get_observers, fun(_, _) -> [] end),
    meck:expect(m_config, get_boolean, fun(_, _, _) -> false end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).

failed_render() ->
    meck:expect(template_compiler, render, fun(_, _, _, _) -> {error, enoent} end),
    ?assertEqual({error, enoent}, z_template:render_to_iolist_result(<<"missing.tpl">>, [], #context{})),
    %% Existing human-facing rendering retains its tolerant behavior.
    {Html, _} = z_template:render_to_iolist(<<"missing.tpl">>, [], #context{}),
    ?assertEqual(<<>>, iolist_to_binary(Html)).

empty_render() ->
    meck:expect(template_compiler, render, fun(_, _, _, _) -> {ok, <<>>} end),
    {ok, Html, _} = z_template:render_to_iolist_result(<<"empty.tpl">>, [], #context{}),
    ?assertEqual(<<>>, iolist_to_binary(Html)).

successful_render() ->
    meck:expect(template_compiler, render, fun(_, Vars, _, _) ->
        ?assertEqual(#{body => <<"content">>}, Vars),
        {ok, [<<"<p>">>, maps:get(body, Vars), <<"</p>">>]}
    end),
    {ok, Html, _} = z_template:render_to_iolist_result(<<"ok.tpl">>, [{body, <<"content">>}], #context{}),
    ?assertEqual(<<"<p>content</p>">>, iolist_to_binary(Html)).

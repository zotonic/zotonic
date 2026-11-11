%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for capture lifecycle, limits, access control and template tracing.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_development_capture_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

lifecycle_test() ->
    {{ok, #{id := Id, state := stopped}}, S0} = z_development_capture:command(
        attach, #{sid => <<"browser">>, path => <<"/example">>}, #{}),
    Render = fun(G) -> G#{nodes => #{one => #{id => <<"one">>}}} end,
    ?assertEqual(S0, z_development_capture:render(<<"browser">>, Render, S0)),
    {{ok, _}, S1} = z_development_capture:command(start, #{id => Id}, S0),
    ?assertEqual(S1, z_development_capture:render(<<"other">>, Render, S1)),
    S2 = z_development_capture:render(<<"browser">>, Render, S1),
    {{ok, #{nodes := [#{id := <<"one">>}]}}, _} = z_development_capture:command(trace, #{id => Id}, S2),
    {{error, badarg}, S2} = z_development_capture:command(poll,
        #{id => Id, sid => <<"other">>, entries => [<<"secret">>]}, S2),
    {{ok, _}, S3} = z_development_capture:command(poll,
        #{id => Id, sid => <<"browser">>, entries => [<<"hello">>]}, S2),
    {{ok, _}, S4} = z_development_capture:command(stop, #{id => Id}, S3),
    ?assertEqual(S4, z_development_capture:render(<<"browser">>, Render, S4)),
    {{ok, _}, S4} = z_development_capture:command(poll,
        #{id => Id, sid => <<"browser">>, entries => [<<"ignored">>]}, S4),
    {{ok, #{entries := [#{text := <<"hello">>}]}}, _} = z_development_capture:command(console, #{id => Id}, S4),
    %% Reads never reveal the browser session id.
    {{ok, Public}, _} = z_development_capture:command(status, #{id => Id}, S4),
    ?assertNot(maps:is_key(sid, Public)),
    Expired = S4#{Id => (maps:get(Id, S4))#{expires => 0}},
    ?assertEqual({{error, enoent}, #{}}, z_development_capture:command(start, #{id => Id}, Expired)).

bounded_console_test() ->
    {{ok, #{id := Id}}, S0} = z_development_capture:command(attach, #{sid => <<"s">>, path => <<"/">>}, #{}),
    {{ok, _}, S1} = z_development_capture:command(start, #{id => Id}, S0),
    S2 = lists:foldl(fun(_, S) ->
        {_, Next} = z_development_capture:command(poll,
            #{id => Id, sid => <<"s">>, entries => lists:duplicate(50, <<"message">>)}, S),
        Next
    end, S1, lists:seq(1, 11)),
    {{ok, #{entries := Entries, truncated := true}}, _} = z_development_capture:command(console, #{id => Id}, S2),
    ?assertEqual(500, length(Entries)),
    ?assertMatch(#{seq := 51}, hd(Entries)),
    ?assertMatch(#{seq := 550}, lists:last(Entries)).

access_test_() ->
    z_test_support:isolated({setup,
        fun() ->
            meck:new([z_acl, m_site], [no_link]),
            meck:expect(z_acl, is_admin, fun(C) -> maps:get(admin, C#context.props, false) end),
            meck:expect(z_acl, is_allowed, fun(use, mod_development, _) -> true end),
            meck:expect(m_site, environment, fun(C) -> maps:get(environment, C#context.props, production) end)
        end,
        fun(_) -> meck:unload([z_acl, m_site]) end,
        fun(_) -> [?_test(begin
            lists:foreach(fun(Env) ->
                ?assert(m_development_capture:console_allowed(context(#{admin => true, environment => Env}))),
                ?assertNot(m_development_capture:console_allowed(context(#{environment => Env})))
            end, [development, test, acceptance]),
            ?assertNot(m_development_capture:console_allowed(context(#{admin => true, environment => production}))),
            ?assertEqual({error, eacces}, m_development_capture:m_get([<<"trace">>, <<"id">>], #{}, context(#{}))),
            ?assertEqual({error, eacces}, m_development_capture:m_post([<<"start">>, <<"id">>], #{}, context(#{}))),
            ?assertEqual({error, eacces}, m_development_capture:m_get([<<"console">>, <<"id">>], #{}, context(#{admin => true}))),
            ?assertEqual({error, eacces}, m_development_capture:m_post([<<"poll">>, <<"id">>],
                #{payload => #{<<"entries">> => [<<"private">>]}}, context(#{admin => false}))),
            ?assertEqual({error, eacces}, m_development_capture:m_post([<<"poll">>, <<"id">>],
                #{payload => #{<<"entries">> => [<<"private">>]}}, context(#{admin => true, environment => production})))
        end)] end}).

read_only_admin_test_() ->
    z_test_support:isolated({setup,
        fun() ->
            %% Keep the real ACL functions, including the trusted sudo exception.
            meck:new([m_site, mod_development], [no_link]),
            meck:expect(m_site, environment, fun(C) -> maps:get(environment, C#context.props) end),
            meck:expect(mod_development, capture, fun(_, _, _) -> {ok, #{}} end)
        end,
        fun(_) -> meck:unload([m_site, mod_development]) end,
        fun(_) -> [?_test(read_only_admin(Env)) || Env <- [development, test, acceptance]] end}).

read_only_admin(Env) ->
    C = #context{site = capture_test, user_id = 1, acl_is_read_only = true,
        props = #{environment => Env, session_id => <<"capture-session">>}},
    Posts = [
        {[<<"attach">>], #{payload => #{<<"path">> => <<"/">>}}},
        {[<<"start">>, <<"id">>], #{}},
        {[<<"stop">>, <<"id">>], #{}},
        {[<<"poll">>, <<"id">>], #{payload => #{<<"entries">> => [<<"console log">>]}}},
        {[<<"poll">>, <<"id">>], #{payload => #{<<"entries">> => []}}}
    ],
    ?assert(z_acl:is_read_only(C)),
    ?assert(m_development_tools:allowed(C)),
    meck:reset(mod_development),
    lists:foreach(fun({Path, Msg}) ->
        ?assertEqual({error, eacces}, m_development_capture:m_post(Path, Msg, C))
    end, Posts),
    ?assertEqual(0, meck:num_calls(mod_development, capture, '_')),
    %% Diagnostics remain readable with a read-only admin token.
    lists:foreach(fun(Path) ->
        ?assertEqual({ok, {#{}, []}}, m_development_capture:m_get(Path, #{}, C))
    end, [[<<"list">>], [<<"status">>, <<"id">>],
        [<<"trace">>, <<"id">>], [<<"console">>, <<"id">>]]),
    %% Ordinary writable admins and the trusted CLI's sudo context still work.
    lists:foreach(fun(Writable) ->
        lists:foreach(fun({Path, Msg}) ->
            ?assertEqual({ok, #{}}, m_development_capture:m_post(Path, Msg, Writable))
        end, Posts)
    end, [z_acl:set_read_only(false, C), z_acl:sudo(C)]).

trace_integration_test() ->
    %% Exercise the actual module callbacks without touching a running site's debugger.
    {ok, S0} = mod_development:init([{context, #context{site = capture_test}}]),
    {reply, {ok, #{id := Id}}, S1} = mod_development:handle_call(
        {capture, attach, #{sid => <<"s">>, path => <<"/">>}}, self(), S0),
    {reply, {ok, _}, S2} = mod_development:handle_call({capture, start, #{id => Id}}, self(), S1),
    {noreply, S3} = mod_development:handle_cast(
        {template_render, {render, <<"/site/priv/templates/page.tpl">>, undefined}, <<"s">>}, S2),
    {reply, {ok, #{nodes := [#{filepath := <<"/site/priv/templates/page.tpl">>}]} = Result}, S4} =
        mod_development:handle_call({capture, trace, #{id => Id}}, self(), S3),
    ?assert(is_binary(z_json:encode(Result))),
    %% Starting the human debugger does not reset the capture graph.
    {noreply, S5} = mod_development:handle_cast({template_trace_start, <<"other">>}, S4),
    {reply, {ok, #{nodes := [_]}}, _} = mod_development:handle_call({capture, trace, #{id => Id}}, self(), S5).

template_syntax_test() ->
    {ok, Source} = file:read_file("apps/zotonic_mod_development/priv/templates/_html_head_development.tpl"),
    {ok, Tokens} = template_compiler_scanner:scan(<<"capture-template">>, Source),
    ?assertMatch({ok, _}, template_compiler_parser:parse(Tokens)).

%% Keep mock flags inside a real context so model contracts are checked too.
-spec context(map()) -> z:context().
context(Props) ->
    #context{site = capture_test, props = Props}.

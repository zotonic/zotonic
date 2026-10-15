%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for development editor discovery and launching.
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

-module(z_development_security_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([tests/1]).

%% @doc Run against a running development site with eunit:test(tests(z:c(Site))).
%% Only rejected launches are exercised; no editor is opened or preference saved.
tests(Context) ->
    Anonymous = z_acl:anondo(Context),
    Admin = z_acl:sudo(Context),
    {ok, #module_index{filepath = File}} = z_module_indexer:find(
        template, <<"admin_development.tpl">>, Admin),
    Base = #{<<"file">> => File, <<"editor">> => <<"auto">>, <<"line">> => 1, <<"column">> => 1},
    [
        {"model reads require development permission", fun() ->
            lists:foreach(fun(Path) ->
                ?assertEqual({error, eacces}, m_development:m_get([Path], undefined, Anonymous))
            end, [<<"editor">>, <<"dispatch_info">>, <<"list_observers">>])
        end},
        {"launch and preference writes require permission", fun() ->
            ?assertEqual({error, eacces}, post(<<"open">>, Base, Anonymous)),
            ?assertEqual({error, eacces}, post(<<"preference">>, #{<<"editor">> => <<"auto">>}, Anonymous))
        end},
        {"arbitrary and traversing files cannot be opened", fun() ->
            lists:foreach(fun(Path) ->
                ?assertEqual({error, enoent}, post(<<"open">>, Base#{<<"file">> => Path}, Admin))
            end, [<<"/etc/passwd">>, <<"/tmp/priv/templates/../../etc/passwd">>,
                  <<"/tmp/priv/templates/admin_development.tpl">>])
        end},
        {"malformed launch fields are rejected", fun() ->
            lists:foreach(fun(Override) ->
                ?assertEqual({error, badarg}, post(<<"open">>, maps:merge(Base, Override), Admin))
            end, [#{<<"file">> => []}, #{<<"line">> => 0}, #{<<"column">> => <<"1">>},
                  #{<<"editor">> => binary:copy(<<"x">>, 128)}, #{<<"editor">> => <<>>}])
        end},
        {"source tokens fail closed and are purpose checked", fun() ->
            Valid = z_crypto:pickle({development_editor_source, File, 1}, Admin),
            <<First, Rest/binary>> = Valid,
            Changed = case First of $A -> $B; _ -> $A end,
            Tampered = <<Changed, Rest/binary>>,
            ?assertEqual({error, eacces}, post(<<"open">>, Base#{<<"source">> => Valid}, Anonymous)),
            WrongPurpose = z_crypto:pickle({unrelated, File, 1}, Admin),
            BadPosition = z_crypto:pickle({development_editor_source, File, 0}, Admin),
            lists:foreach(fun(Token) ->
                ?assertEqual({error, eacces}, post(<<"open">>, Base#{<<"source">> => Token}, Admin))
            end, [WrongPurpose, BadPosition, Tampered]),
            ?assertEqual({error, badarg}, post(<<"open">>, Base#{<<"source">> => []}, Admin)),
            ?assertEqual({error, badarg}, post(<<"open">>, Base#{<<"source">> => <<>>}, Admin))
        end},
        {"template viewer handles malformed paths without reading files", fun() ->
            lists:foreach(fun(Path) ->
                ?assertMatch(#context{}, mod_development:event(
                    #postback{message = {template_view, [{template, Path}]}}, Admin))
            end, [[], #{}, 42, binary:copy(<<"x">>, 4096), <<"/etc/passwd">>])
        end},
        {"callbacks reject anonymous callers before processing input", fun() ->
            ?assertMatch(#context{}, mod_development:event(
                #postback{message = {template_view, [{template, #{}}]}}, Anonymous)),
            ?assertMatch(#context{}, z_development_template:event(
                #postback{message = {template_matches, [{template, #{}}]}}, Anonymous))
        end}
    ].

post(Action, Payload, Context) ->
    m_development:m_post([<<"editor">>, Action], #{payload => Payload}, Context).

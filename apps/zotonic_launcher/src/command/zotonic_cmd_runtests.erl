%%%-------------------------------------------------------------------
%%% @author Blaise
%%% @doc
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%
%%% @end
%%% Created : 18. Dec 2017 12:06 PM
%%%-------------------------------------------------------------------
-module(zotonic_cmd_runtests).
-author("Blaise").

%% API
-export([run/1]).

-include("../../include/zotonic_command.hrl").

run(_) ->
    net_kernel:start([zotonic_runtests, shortnames]),
    erlang:set_cookie(node(), erlang:get_cookie()),
    Target = list_to_atom(?NODENAME ++ "@" ++ ?NODEHOST),
    case net_adm:ping(Target) of
        pang ->
            Cmd = case zotonic_command:base_cmd_test() of
                {ok, BaseCmd} ->
                    BaseCmd
                    ++ " -sasl errlog_type error -s zotonic "
                    ++ "-eval 'zotonic:await_startup(),init:stop(case eunit:test(["
                        ++ tests()
                        ++ "],[]) of error -> 1; ok -> 0 end)'";
                {error, ErrCmd} ->
                    ErrCmd
            end,
            io:format("~s", [ Cmd ]);
        pong ->
            io:format("echo Zotonic should not be running when running tests; exit 2")
    end.

tests() ->
    Beams = filelib:wildcard( filename:join([ ?ZOTONIC, "_build", "default", "lib", "zotonic_*", "test", "*.beam" ])),
    Bs = lists:map(
        fun(B) ->
            filename:rootname( filename:basename(B) )
        end,
        Beams),
    concat(Bs, "").

concat([], Acc) ->
    Acc;
concat([ B | Bs ], "") ->
    concat(Bs, B);
concat([ B | Bs ], Acc) ->
    concat(Bs, Acc ++ "," ++ B).

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

run(Args) ->
    Tests = Args -- [ "runtests" ],
    {ok, BaseCmd} = zotonic_command:base_cmd_test(),
    Cmd = BaseCmd
        ++ " -sasl errlog_type error -s zotonic "
        ++ "-eval 'zotonic:runtests(["++tests(Tests)++"])'",
    io:format("~s", [ Cmd ]).

tests(Args) ->
    ZotonicDir = zotonic_command:get_zotonic_dir(),
    Beams = filelib:wildcard( filename:join([ ZotonicDir, "_build", "default", "lib", "zotonic_*", "test", "*.erl" ])),
    Bs = lists:map(
        fun(B) ->
            filename:rootname( filename:basename(B) )
        end,
        Beams),
    concat( filter_tests(Bs, Args), "").

filter_tests(Bs, []) ->
    Bs;
filter_tests(Bs, Args) ->
    lists:filter(
        fun(B) -> lists:member(B, Args) end,
        Bs).

concat([], Acc) ->
    Acc;
concat([ B | Bs ], "") ->
    concat(Bs, B);
concat([ B | Bs ], Acc) ->
    concat(Bs, Acc ++ "," ++ B).

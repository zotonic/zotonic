%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 20019 Marc Worrell
%% @doc Model for supporting the development views.

%% Copyright 2019 Marc Worrell
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

-module(m_development).

-export([
    m_find_value/3
    ]).

-include("zotonic.hrl").

m_find_value(list_observers, #m{ value = undefined }, Context) ->
    Observers = z_notifier:get_observers(Context),
    [ {atom_to_binary(Event, utf8), readable(Os)} || {Event, Os} <- Observers ].

readable(Os) ->
    lists:map(
        fun({Prio, Obs}) ->
            {Prio, iolist_to_binary(readable_1(Obs))}
        end,
        Os).

readable_1(Pid) when is_pid(Pid) ->
    z_html:escape( iolist_to_binary( io_lib:format("~p", [Pid]) ) );
readable_1(F) when is_function(F) ->
    <<"<i>function</i>">>;
readable_1({M, F}) ->
    [
        "<b>", atom_to_binary(M, utf8), "</b>",
        ":", atom_to_binary(F, utf8)
    ];
readable_1({M, F, Args}) ->
    As = [ io_lib:format("~p", [A]) || A <- Args ],
    As1 = z_utils:combine(", ", As),
    [
        "<b>", z_html:escape(atom_to_binary(M, utf8)), "</b>",
        ":", z_html:escape(atom_to_binary(F, utf8)),
        "(", z_html:escape(iolist_to_binary(As1)), ")"
    ].


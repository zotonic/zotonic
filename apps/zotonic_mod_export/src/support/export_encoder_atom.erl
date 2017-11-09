%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for atom format

%% Copyright 2017 Marc Worrell
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

-module(export_encoder_atom).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    id
}).

-export([
    extension/0,
    mime/0,
    init/2,
    header/3,
    row/3,
    footer/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

extension() ->
    [ <<"atom">> ].

mime() ->
    [ <<"application/atom+xml">> ].

init(Options, _Context) ->
    {ok, #state{
        id = proplists:get_value(id, Options)
    }}.

header(Header, #state{id=Id} = State, Context) ->
    Vars = [
        {id, Id},
        {title, Header}
    ],
    {Bin, _Context} = z_template:render_to_iolist("atom/feed_start.tpl", Vars, Context),
    {ok, iolist_to_binary(Bin), State}.

row(Row, #state{} = State, Context) when is_integer(Row) ->
    case m_rsc:is_visible(Row, Context) of
        true ->
            Vars = [
                {id, Row}
            ],
            {Bin, _Context} = z_template:render_to_iolist({cat, "atom/entry.tpl"}, Vars, Context),
            {ok, iolist_to_binary(Bin), State};
        false ->
            {ok, <<>>, State}
    end;
row(Row, #state{} = State, Context) ->
    Vars = [
        {id, Row}
    ],
    {Bin, _Context} = z_template:render_to_iolist("atom/entry.tpl", Vars, Context),
    {ok, iolist_to_binary(Bin), State}.

footer(_Data, #state{}, Context) ->
    {Bin, _Context} = z_template:render_to_iolist("atom/feed_end.tpl", [], Context),
    {ok, iolist_to_binary(Bin)}.

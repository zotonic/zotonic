%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for vevent format

%% Copyright 2016 Marc Worrell
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

-module(export_encoder_ics).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    id,
    header_template,
    row_template
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
    [ <<"ics">> ].

mime() ->
    [ {<<"text">>, <<"calendar">>, []} ].

init(Options, Context) ->
    {ok, #state{
        id = proplists:get_value(id, Options),
        header_template = z_context:get(header_template, Context, "_vcalendar_header.tpl"),
        row_template = z_context:get(row_template, Context, "_vevent.tpl")
    }}.

header(Header, #state{id=Id, header_template=Template} = State, Context) ->
    Vars = [
        {id, Id},
        {title, Header}
    ],
    {Bin, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    {ok, iolist_to_binary(Bin), State}.

row(Row, #state{row_template=Template} = State, Context) when is_integer(Row) ->
    Vars = [
        {id, Row}
    ],
    {Bin, _Context} = z_template:render_to_iolist({cat, Template}, Vars, Context),
    {ok, iolist_to_binary(Bin), State};
row(Row, #state{row_template=Template} = State, Context) ->
    Vars = [
        {id, Row}
    ],
    {Bin, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    {ok, iolist_to_binary(Bin), State}.

footer(_Data, #state{}, _Context) ->
    {ok, <<"END:VCALENDAR\n">>}.

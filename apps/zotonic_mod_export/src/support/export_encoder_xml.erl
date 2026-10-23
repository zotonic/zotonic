%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2026 Marc Worrell <marc@worrell.nl>
%% @doc Shared streaming template encoder for Atom and RSS feeds
%% @end

%% Copyright 2017-2026 Marc Worrell
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

-module(export_encoder_xml).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    id,
    dispatch,
    format = atom,
    updated = {{1970, 1, 1}, {0, 0, 0}}
}).

-export([
    init/2,
    header/3,
    row/3,
    footer/3
]).

init(Options, Context) ->
    Id = proplists:get_value(id, Options),
    State = #state{
        id = Id,
        format = proplists:get_value(feed_format, Options, atom),
        dispatch = proplists:get_value(dispatch, Options, export_rsc)
    },
    {ok, update_modified(Id, State, Context)}.

header(Header, #state{id=Id, dispatch=Dispatch, format=Format} = State, Context) ->
    Vars = [
        {id, Id},
        {feed_url, export_feed:url(Dispatch, Id, Format, Context)},
        {title, Header}
    ],
    {Bin, _Context} = z_template:render_to_iolist(template(Format, "feed_start.tpl"), Vars, Context),
    {ok, iolist_to_binary(Bin), State}.

row(Row, #state{format=Format} = State, Context) when is_integer(Row) ->
    case m_rsc:is_visible(Row, Context) of
        true ->
            Vars = row_vars(Format, Row, Context),
            {Bin, _Context} = z_template:render_to_iolist({cat, template(Format, "entry.tpl")}, Vars, Context),
            {ok, iolist_to_binary(Bin), update_modified(Row, State, Context)};
        false ->
            {ok, <<>>, State}
    end;
row(Row, #state{format=Format} = State, Context) ->
    Vars = row_vars(Format, Row, Context),
    {Bin, _Context} = z_template:render_to_iolist(template(Format, "entry.tpl"), Vars, Context),
    {ok, iolist_to_binary(Bin), update_modified(Row, State, Context)}.

footer(_Data, #state{updated=Updated, format=Format}, Context) ->
    % Atom permits feed metadata after the entries. This keeps the export streaming.
    Vars = [
        {next_url, z_context:get(feed_next_url, Context)},
        {updated, Updated},
        {updated_rfc822, rss_date(Updated)}
    ],
    {Bin, _Context} = z_template:render_to_iolist(template(Format, "feed_end.tpl"), Vars, Context),
    {ok, iolist_to_binary(Bin)}.

template(Format, Name) ->
    atom_to_list(Format) ++ "/" ++ Name.

update_modified(Id, State, Context) when is_integer(Id) ->
    update_modified_value(m_rsc:p(Id, modified, Context), State);
update_modified(Row, State, _Context) when is_map(Row) ->
    update_modified_value(maps:get(<<"modified">>, Row, maps:get(modified, Row, undefined)), State);
update_modified(_Row, State, _Context) ->
    State.

update_modified_value({{_, _, _}, {_, _, _}} = Modified, #state{updated=Updated} = State) ->
    State#state{updated = max(Modified, Updated)};
update_modified_value(_Modified, State) ->
    State.

%% RSS dates must use English weekday/month names regardless of feed language.
row_vars(rss, Row, Context) ->
    Published = case Row of
        N when is_integer(N) -> m_rsc:p(N, <<"publication_start">>, Context);
        M when is_map(M) -> maps:get(<<"publication_start">>, M, undefined);
        _ -> undefined
    end,
    [
        {id, Row},
        {published_rfc822, rss_date(Published)}
    ];
row_vars(_, Row, _) ->
    [
        {id, Row}
    ].

rss_date({{_, _, _}, {_, _, _}} = Date) ->
    z_dateformat:format(Date, "D, d M Y H:i:s O", [{utc, Date}, {tz, "GMT"}]);
rss_date(_) -> undefined.

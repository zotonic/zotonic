%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for vevent format
%% @end

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

extension() ->
    [ <<"ics">>, <<"ical">> ].

mime() ->
    [ {<<"text">>, <<"calendar">>, []} ].

init(Options, Context) ->
    {ok, #state{
        id = proplists:get_value(id, Options),
        header_template = z_context:get(header_template, Context, "ical/feed_start.tpl"),
        row_template = z_context:get(row_template, Context, "ical/entry.tpl")
    }}.

header(Header, #state{id=Id, header_template=Template} = State, Context) ->
    Vars = [
        {id, Id},
        {title, Header},
        {next_url, z_context:get(feed_next_url, Context)}
    ],
    {Bin, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    {ok, content_lines(Bin), State}.

row(Row, #state{row_template=Template} = State, Context) ->
    Visible = case Row of
        Id when is_integer(Id) -> m_rsc:is_visible(Id, Context);
        Props when is_map(Props) -> maps:get(<<"is_visible">>, Props, false);
        _ -> false
    end,
    Start = prop(Row, date_start, Context),
    Uri = prop(Row, uri, Context),
    Modified = prop(Row, modified, Context),
    case Visible andalso valid_date(Start) andalso valid_date(Modified)
            andalso is_binary(Uri) andalso Uri =/= <<>> of
        true ->
            End = prop(Row, date_end, Context),
            End1 = case valid_date(End) andalso End >= Start of
                true -> End;
                false -> Start
            end,
            Created = prop(Row, created, Context),
            Created1 = case valid_date(Created) of
                true -> Created;
                false -> undefined
            end,
            Vars = [
                {created, Created1},
                {id, Row},
                {dtstart, Start},
                {dtend, End1},
                {dtstamp, Modified},
                {resource_url, resource_url(Row, Context)}
            ],
            Tpl = case is_integer(Row) of true -> {cat, Template}; false -> Template end,
            {Bin, _} = z_template:render_to_iolist(Tpl, Vars, Context),
            {ok, content_lines(Bin), State};
        false ->
            {ok, <<>>, State}
    end.

prop(Id, Key, Context) when is_integer(Id) -> m_rsc:p(Id, Key, Context);
prop(Props, Key, _) when is_map(Props) -> maps:get(atom_to_binary(Key), Props, undefined);
prop(_, _, _) -> undefined.

resource_url(Id, Context) when is_integer(Id) ->
    z_dispatcher:url_for(api_rsc_export, [{id, Id}, {absolute_url, true}], none, Context);
resource_url(_, _) -> undefined.

valid_date({{Y, _, _}, _} = DT) when is_integer(Y), Y > 0, Y < 9999 ->
    z_datetime:undefined_if_invalid_date(DT) =/= undefined;
valid_date(_) -> false.

%% Fold complete content lines to 75 octets without splitting UTF-8 characters.
%% Unfold first to support older site templates which use escape_ical.
content_lines(Data) ->
    Bin = re:replace(iolist_to_binary(Data), <<"\\r?\\n[ \\t]">>, <<>>, [global, {return, binary}]),
    Lines = binary:split(binary:replace(Bin, <<"\r\n">>, <<"\n">>, [global]), <<"\n">>, [global]),
    iolist_to_binary([[fold(Line, 0), <<"\r\n">>] || Line <- Lines, Line =/= <<>>]).
fold(<<>>, _) -> [];
fold(<<C/utf8, Rest/binary>>, N) ->
    Char = <<C/utf8>>,
    case N + byte_size(Char) > 75 of
        true -> [<<"\r\n ">>, Char | fold(Rest, 1 + byte_size(Char))];
        false -> [Char | fold(Rest, N + byte_size(Char))]
    end.

footer(_Data, #state{}, _Context) ->
    {ok, <<"END:VCALENDAR\r\n">>}.

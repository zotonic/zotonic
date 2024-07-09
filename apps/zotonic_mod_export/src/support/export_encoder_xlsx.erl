%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2024 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for Microsoft xlsx format
%% @end

%% Copyright 2016-2024 Marc Worrell
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

-module(export_encoder_xlsx).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    props :: list(atom()) | undefined,
    rows = [] :: list(),
    is_raw = false
    }).

-export([
    extension/0,
    mime/0,
    init/2,
    header/3,
    row/3,
    footer/3
]).

% For testing
-export([
    number_to_letter/1,
    zip/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

extension() ->
    [ <<"xlsx">> ].

mime() ->
    [ {<<"application">>, <<"vnd.openxmlformats-officedocument.spreadsheetml.sheet">>, []} ].

init(Options, Context) ->
    IsRaw = proplists:get_value(is_raw, Options, false),
    case z_context:get(rsc_props, Context) of
        L when is_list(L) ->
            {ok, #state{ props=L, is_raw=IsRaw}};
        undefined ->
            {ok, #state{ props=undefined, is_raw=IsRaw }}
    end.

header(undefined, #state{props=undefined} = State, Context) ->
    Ps = mod_export:rsc_props(Context) -- [blocks],
    {ok, <<>>, State#state{props=Ps}};
header(undefined, #state{props=Ps} = State, _Context) ->
    {ok, <<>>, State#state{props=Ps}};
header(Row, State, _Context) ->
    {ok, <<>>, State#state{props=Row}}.

row(Row, #state{rows=Rows} = State, _Context) ->
    {ok, <<>>, State#state{rows=[Row|Rows]}}.

footer(_Row, State, Context) ->
    zip(State#state.props, lists:reverse(State#state.rows), State#state.is_raw, Context).

zip(Keys, Rows, IsRaw, Context) ->
    Vars = [
        {encode_cell, fun (V, Ctx) -> encode_cell(V, IsRaw, Ctx) end},
        {lookup_header, fun export_encoder:lookup_header/2},
        {lookup_value, fun export_encoder:lookup_value/2},
        {sheet_name, <<"Sheet1">>},
        {keys, Keys},
        {rows, Rows}
    ],
    Fs = lists:map(fun({Fn, Template}) ->
                     {Fn, file_render(Template, Vars, Context)}
                   end,
                   file_templates()),
    {ok, {_ZipFilename, ZipBin}} = zip:zip("", Fs, [memory]),
    {ok, ZipBin}.

file_render(Template, Vars, Context) ->
    {Bin, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    iolist_to_binary(Bin).

file_templates() ->
    [
        {"_rels/.rels", <<"xlsx/_rels.xml.tpl">>},
        {"xl/_rels/workbook.xml.rels", <<"xlsx/workbook.xml.rels.tpl">>},
        {"xl/worksheets/sheet1.xml", <<"xlsx/sheet1.xml.tpl">>},
        {"xl/styles.xml", <<"xlsx/styles.xml.tpl">>},
        {"xl/workbook.xml", <<"xlsx/workbook.xml.tpl">>},
        {"[Content_Types].xml", <<"xlsx/content_types.xml.tpl">>}
    ].

encode_cell([Row, Col, V], _IsRaw, _Context) when is_integer(V); is_float(V) ->
    iolist_to_binary([
        <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), <<"\">">>,
            <<"<v>">>, z_convert:to_binary(V), <<"</v></c>">>
        ]);
encode_cell([Row, Col, ?ST_JUTTEMIS], IsRaw, Context) ->
    encode_cell([Row, Col, <<>>], IsRaw, Context);
encode_cell([Row, Col, {{Y,M,D},{H,I,S}} = Date], _IsRaw, _Context) when
        is_integer(Y), is_integer(M), is_integer(D),
        is_integer(H), is_integer(I), is_integer(S) ->
    try
        Secs = z_datetime:datetime_to_timestamp(Date) + 2209161600,
        iolist_to_binary([
            <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), <<"\" s=\"2\">">>,
                <<"<v>">>, z_convert:to_binary(Secs / 86400), <<"</v></c>">>
            ])
    catch
        error:_if_clause ->
            ?LOG_WARNING(#{
                text => <<"Illegal date in xlsx export">>,
                in => zotonic_mod_export,
                result => error,
                reason => illegal_date,
                date => Date
            }),
            encode_inlinestr(Row, Col, <<>>)
    end;
encode_cell([Row, Col, {Y,M,D} = Date], _IsRaw, _Context) when
        is_integer(Y), is_integer(M), is_integer(D) ->
    try
        Secs = z_datetime:datetime_to_timestamp({Date, {0,0,0}}) + 2209161600,
        iolist_to_binary([
            <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), <<"\" s=\"2\">">>,
                <<"<v>">>, z_convert:to_binary(Secs / 86400), <<"</v></c>">>
            ])
    catch
        error:_if_clause ->
            ?LOG_WARNING(#{
                text => <<"Illegal date in xlsx export">>,
                in => zotonic_mod_export,
                result => error,
                reason => illegal_date,
                date => Date
            }),
            encode_inlinestr(Row, Col, <<>>)
    end;
encode_cell([Row, Col, V], false, _Context) ->
    B = z_xml:escape(
            z_html:unescape(
                z_html:strip(z_convert:to_binary(V)))),
    encode_inlinestr(Row, Col, B);
encode_cell([Row, Col, V], true, _Context) ->
    B = z_xml:escape(z_convert:to_binary(V)),
    encode_inlinestr(Row, Col, B).

encode_inlinestr(Row, Col, B) ->
    iolist_to_binary([
        <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), <<"\" t=\"inlineStr\">">>,
            <<"<is><t>">>, B, <<"</t></is></c>">>
        ]).


number_to_letter(0) ->
    <<>>;
number_to_letter(N) when N > 0 ->
    number_to_letter(N, <<>>).

number_to_letter(0, Acc) ->
    Acc;
number_to_letter(N, Acc) ->
    C = ((N-1) rem 26) + $A,
    number_to_letter((N-1) div 26, <<C,Acc/binary>>).

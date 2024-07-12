%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2024 Marc Worrell
%% @doc Interface with the export encoders.
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

-module(export_encoder).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    content_types/1,
    content_types_dispatch/2,
    stream/4,

    lookup_header/2,
    lookup_value/2
    ]).

-record(stream_state, {
        id,
        is_query,
        content_type,
        dispatch,
        encoder,
        encoder_state,
        exporter_state
    }).

-include_lib("zotonic_core/include/zotonic.hrl").


content_types(_Context) ->
    lists:usort(
        lists:foldl(
            fun(Encoder, Acc) ->
                Acc ++ Encoder:mime()
            end,
            [],
            encoders())).

%% todo: let the encoder check against the category of the id
content_types_dispatch(_Id, _Context) ->
    lists:flatten(
        lists:map(
            fun(Encoder) ->
                Mimes = Encoder:mime(),
                Type = hd(Encoder:extension()),
                [ {Mime, {export_rsc, [{type, Type}]}} || Mime <- Mimes ]
            end,
            encoders())).

stream(Id, ContentType, Options, Context) ->
    Enc = select_encoder(ContentType, encoders()),
    EncOpts = [
        {id, Id},
        {dispatch, proplists:get_value(dispatch, Options)},
        {is_query, proplists:get_value(is_query, Options, false)},
        {is_raw, proplists:get_value(is_raw, Options, false)}
    ],
    {ok, EncState} = Enc:init(EncOpts, Context),
    StreamState = #stream_state{
        id = Id,
        is_query = proplists:get_value(is_query, EncOpts),
        content_type = ContentType,
        dispatch = proplists:get_value(dispatch, EncOpts),
        encoder = Enc,
        encoder_state = EncState
    },
    {stream, {<<>>, fun() -> do_header(StreamState, Context) end}}.

select_encoder(_CT, []) ->
    export_encoder_csv;
select_encoder(CT, [M|Ms]) ->
    case lists:member(CT, M:mime()) of
        true -> M;
        false -> select_encoder(CT,Ms)
    end.

encoders() ->
    [
        export_encoder_json,
        export_encoder_csv,
        export_encoder_xlsx,
        export_encoder_ics,
        export_encoder_atom,
        export_encoder_bert,
        export_encoder_ubf
    ].

do_header(StreamState, Context) ->
    case export_helper:call(#export_resource_header{
                                id=StreamState#stream_state.id,
                                content_type=StreamState#stream_state.content_type,
                                dispatch=StreamState#stream_state.dispatch
                          },
                          Context)
    of
        undefined ->
            do_header_1(undefined, StreamState, Context);
        {ok, Header, State} ->
            do_header_1(Header, StreamState#stream_state{exporter_state=State}, Context);
        {ok, Header} ->
            do_header_1(Header, StreamState#stream_state{exporter_state=undefined}, Context)
    end.

do_header_1(Header, #stream_state{encoder=E, encoder_state=ES} = StreamState, Context) ->
    {ok, Data, ES1} = E:header(Header, ES, Context),
    StreamState1 = StreamState#stream_state{encoder_state=ES1},
    case is_empty(Data) of
        true -> do_body(StreamState1, Context);
        false -> {Data, fun() -> do_body(StreamState1, Context) end}
    end.

is_empty(undefined) -> true;
is_empty([]) -> true;
is_empty(<<>>) -> true;
is_empty(_) -> false.


do_body(#stream_state{is_query=true, id=Id} = StreamState, Context) ->
    #search_result{result=Ids} = z_search:search(
            <<"query">>, #{ <<"query_id">> => Id },
            1, 30000,
            Context),
    do_body_data(Ids, StreamState, Context);
do_body(StreamState, Context) ->
    case export_helper:call(#export_resource_data{
                                id=StreamState#stream_state.id,
                                content_type=StreamState#stream_state.content_type,
                                dispatch=StreamState#stream_state.dispatch,
                                state=StreamState#stream_state.exporter_state
                          },
                          Context)
    of
        undefined ->
            do_body_data([StreamState#stream_state.id], StreamState, Context);
        {ok, List} ->
            do_body_data(List, StreamState, Context);
        {ok, List, NewState} ->
            do_body_data(List, StreamState#stream_state{exporter_state=NewState}, Context)
    end.

do_body_data([], StreamState, Context) ->
    do_footer(StreamState, Context);
do_body_data(List, StreamState, Context) ->
    {Data, NewStreamState} = lists:foldl(
                                fun(D, {Acc, AccState}) ->
                                    {DEnc, AccState1} = body_encode_row(D, AccState, Context),
                                    {[Acc, DEnc], AccState1}
                                end,
                                {[], StreamState},
                                List),
    DataBin = iolist_to_binary(Data),
    NextFun = case NewStreamState#stream_state.exporter_state of
                  undefined -> fun() -> do_footer(NewStreamState, Context) end;
                  _ -> fun() -> do_body(NewStreamState, Context) end
              end,
    case is_empty(DataBin) of
        true -> NextFun();
        false -> {DataBin, NextFun}
    end.

%% TODO: replace 'encode' with 'row' -> it is about a row of values for a resource
body_encode_row(Item, StreamState, Context) ->
    case export_helper:call(#export_resource_encode{
                                id=StreamState#stream_state.id,
                                content_type=StreamState#stream_state.content_type,
                                dispatch=StreamState#stream_state.dispatch,
                                state=StreamState#stream_state.exporter_state,
                                data=Item
                          },
                          Context)
    of
        undefined ->
            encode_data_row(Item, StreamState, Context);
        {ok, Row} ->
            encode_data_row(Row, StreamState, Context);
        {ok, Row, ExporterState} ->
            encode_data_row(Row, StreamState#stream_state{exporter_state=ExporterState}, Context)
    end.

encode_data_row(RowOrId, #stream_state{encoder=E, encoder_state=ES} = StreamState, Context) ->
    {ok, Data, ES1} = E:row(RowOrId, ES, Context),
    {Data, StreamState#stream_state{encoder_state=ES1}}.


do_footer(#stream_state{encoder=E, encoder_state=ES} = StreamState, Context) ->
    case export_helper:call(#export_resource_footer{
                                id=StreamState#stream_state.id,
                                dispatch=StreamState#stream_state.dispatch,
                                content_type=StreamState#stream_state.content_type,
                                state=StreamState#stream_state.exporter_state},
                          Context)
    of
        undefined ->
            {ok, Data} = E:footer(undefined, ES, Context),
            {Data, done};
        {ok, Row} ->
            {ok, Data} = E:footer(Row, ES, Context),
            {Data, done}
    end.


%% @doc Used to lookup the header name, a header might be a tuple with lookup key and a textual representation.
lookup_header({trans, _} = Tr, Context) ->
    z_trans:lookup_fallback(Tr, Context);
lookup_header({_Key, Header}, Context) ->
    lookup_header(Header, Context);
lookup_header(Key, _Context) ->
    z_convert:to_binary(Key).

%% @doc Used to lookup the row value, a key might be a tuple with lookup key and a textual representation.
lookup_value([Id, {Key, _Header}], Context) ->
    z_template_compiler_runtime:find_value(Key, Id, #{}, Context);
lookup_value([Id, Key], Context) ->
    z_template_compiler_runtime:find_value(Key, Id, #{}, Context).


%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2020 Maas-Maarten Zeemane
%% @doc Interface with the export encoders.

%% Copyright 2020 Maas-Maarten Zeeman 
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


-module(export_encoder_json).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include_lib("zotonic.hrl").

-record(state, {
    props :: list(atom()) | undefined,
    is_raw = false,
    prefix_comma = false
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
    [ "json" ].


mime() ->
    [ "application/json" ].

init(Options, Context) ->
    IsRaw = proplists:get_value(is_raw, Options, false),
    case z_context:get(rsc_props, Context) of
        L when is_list(L) ->
            {ok, #state{ props=L, is_raw=IsRaw }};
        undefined ->
            {ok, #state{ props=undefined, is_raw=IsRaw }}
    end.

header(undefined, #state{props=undefined}=State, Context) ->
    Ps = mod_export:rsc_props(Context) -- [blocks],
    {ok, <<"[">>, State#state{props=Ps}};
header(undefined, #state{props=Ps}=State, _Context) ->
    {ok, <<"[">>, State#state{props=Ps}};
header(Row, #state{is_raw=IsRaw}=State, _Context) ->
    Data = optional_prefix_comma(json_encode(Row, IsRaw), State),
    {ok, Data, State#state{prefix_comma=true}}.

row(Id, #state{props=Ps, is_raw=IsRaw} = State, Context) when is_integer(Id), is_list(Ps) ->
    Data = [ export_encoder:lookup_value([Id, P], Context) || P <- Ps ],
    Props = lists:zip(Ps, Data),
    Data1 = optional_prefix_comma(json_encode(Props, IsRaw), State),
    {ok, Data1, State#state{prefix_comma=true}};
row(Row, #state{is_raw=IsRaw} = State, _Context) ->
    Data = optional_prefix_comma(json_encode(Row, IsRaw), State),
    {ok, Data, State#state{prefix_comma=true}}.

footer(undefined, #state{}, _Context) ->
    {ok, <<"]">>};
footer(Row, #state{is_raw=IsRaw}=State, _Context) ->
    Data = optional_prefix_comma(json_encode(Row, IsRaw), State),
    {ok, <<Data/binary, "]">>}.


json_encode(Data, true) -> Data;
json_encode(Data, false) ->
    try jsxrecord:encode(Data) of
        Json -> Json
    catch
        _:_ ->
            ?DEBUG({error, Data})
    end.


optional_prefix_comma(Data, #state{prefix_comma=true}) -> <<",", Data/binary>>;
optional_prefix_comma(Data, #state{}) -> Data.


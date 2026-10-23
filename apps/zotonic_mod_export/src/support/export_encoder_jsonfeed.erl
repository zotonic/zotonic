%% @copyright 2026 Marc Worrell
%% @doc Stream JSON Feed 1.1 without buffering its items.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(export_encoder_jsonfeed).

-export([extension/0, mime/0, init/2, header/3, row/3, footer/3]).

-record(state, {id, dispatch, is_first = true}).

extension() -> [<<"jsonfeed">>].
mime() -> [{<<"application">>, <<"feed+json">>, []}].

init(Options, _Context) ->
    {ok, #state{id = proplists:get_value(id, Options),
                dispatch = proplists:get_value(dispatch, Options, export_rsc)}}.

header(Header, #state{id=Id, dispatch=Dispatch} = State, Context) ->
    Metadata = z_json:encode(export_feed:json_header(Id, Dispatch, Header, Context)),
    % Leave the object open for the streamed items array.
    Prefix = binary:part(Metadata, 0, byte_size(Metadata) - 1),
    {ok, [Prefix, <<",\"items\":[">>], State}.

row(Row, #state{is_first=IsFirst} = State, Context) ->
    case export_feed:json_item(Row, Context) of
        skip -> {ok, <<>>, State};
        {ok, Item} ->
            Separator = case IsFirst of true -> <<>>; false -> <<",">> end,
            {ok, [Separator, z_json:encode(Item)], State#state{is_first=false}}
    end.

footer(_Data, _State, _Context) -> {ok, <<"]}">>}.

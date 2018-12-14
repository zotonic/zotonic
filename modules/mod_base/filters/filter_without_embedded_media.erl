%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc 'show_media' filter, show the media inserted with the html editor.

%% Copyright 2010 Arjan Scherpenisse
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

-module(filter_without_embedded_media).
-export([without_embedded_media/3]).

-include_lib("zotonic.hrl").


without_embedded_media(undefined, _Id, _Context) ->
    undefined;
without_embedded_media(Input, Id, Context) ->
    case erlydtl_runtime:to_list(Input, Context) of
        [] ->
            [];
        InputIds ->
            BodyIds = text_ids(m_rsc:p(Id, body, Context), Context),
            BlockIds = block_ids(m_rsc:p(Id, blocks, Context), Context),
            AllIds = lists:flatten([ BodyIds, BlockIds ]),
            InputIds -- AllIds
    end.

block_ids(undefined, _Context) ->
    [];
block_ids(Bs, Context) when is_list(Bs) ->
    lists:map(
        fun(B) ->
            text_ids(proplists:get_value(body, B), Context)
        end,
        Bs).

text_ids({trans, _} = Tr, Context) ->
    text_ids(z_trans:lookup_fallback(Tr, Context), Context);
text_ids(Text, _Context) when is_binary(Text) ->
    case re:run(Text, <<"\<\!-- z-media ([0-9]+) ">>, [global, {capture, all_but_first, binary}]) of
        nomatch ->
            [];
        {match, L} ->
            [ binary_to_integer(I) || [I] <- L ]
    end;
text_ids(undefined, _Context) ->
    [].


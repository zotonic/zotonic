%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2017 Arjan Scherpenisse
%% @doc 'embedded_media' filter, return media in the resource body texts

%% Copyright 2010-2017 Arjan Scherpenisse
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

-module(filter_embedded_media).
-export([
    embedded_media/2,
    embedded_media/3
    ]).

-include_lib("zotonic.hrl").


embedded_media(Id, Context) ->
    embedded_media(Id, true, Context).

embedded_media(undefined, _IsCheckBlocks, _Context) ->
    [];
embedded_media({trans, _} = Tr, _IsCheckBlocks, Context) ->
    embedded_media_1(Tr, Context);
embedded_media(Text, _IsCheckBlocks, Context) when is_binary(Text) ->
    embedded_media_1(Text, Context);
embedded_media(Id, IsCheckBlocks0, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> [];
        RscId ->
            IsCheckBlocks = z_convert:to_bool(IsCheckBlocks0),
            MediaIds = embedded_media_1(m_rsc:p(RscId, body, Context), Context)
                ++ embedded_media_1(m_rsc:p(RscId, body_extra, Context), Context)
                ++ embedded_media_blocks_1(IsCheckBlocks, m_rsc:p(RscId, blocks, Context), Context),
            unique(MediaIds)
    end.

unique(MediaIds) ->
    lists:reverse(
        lists:foldl(
            fun(Id, Acc) ->
                case lists:member(Id, Acc) of
                    true -> Acc;
                    false -> [Id|Acc]
                end
            end,
            [],
            MediaIds)).

embedded_media_blocks_1(false, _Blocks, _Context) ->
    [];
embedded_media_blocks_1(true, Blocks, Context) when is_list(Blocks) ->
    lists:flatten(
        lists:map(
            fun(Block) ->
                embedded_media_1(proplists:get_value(body, Block), Context)
            end,
            Blocks));
embedded_media_blocks_1(true, _Blocks, _Context) ->
    [].

embedded_media_1(undefined, _Context) ->
    [];
embedded_media_1({trans, _} = Tr, Context) ->
    embedded_media_1(z_trans:lookup_fallback(Tr, Context), Context);
embedded_media_1(Text, _Context) when is_binary(Text) ->
    case re:run(Text, "\\<\\!-- z-media ([0-9]+) ", [global, {capture, all_but_first, binary}]) of
        nomatch -> [];
        {match, L} -> [binary_to_integer(I) || [I] <- L]
    end;
embedded_media_1(Text, Context) ->
    embedded_media_1(z_convert:to_binary(Text), Context).

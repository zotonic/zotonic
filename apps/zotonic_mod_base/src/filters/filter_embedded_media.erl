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
-moduledoc("
See also

[show\\_media](/id/doc_template_filter_filter_show_media), [without\\_embedded\\_media](/id/doc_template_filter_filter_without_embedded_media), [media\\_for\\_language](/id/doc_template_filter_filter_media_for_language)

Fetch media ids that are embedded in the `body`, `body_extra` and *text* blocks of your page.

This filter lets you loop over every image that is embedded in the texts of the given page:


```django
{% for media_id in id|embedded_media %}
    {% media media_id width=315 extent %}
{% endfor %}
```

Note that all translations of the texts are checked. This makes it possible to add language-dependent media (with a
text, or video) without them showing up as extra depictions with other translations.

There is an optional (boolean) argument to only fetch media ids from the `body` and `body_extra` properties:


```django
{% for media_id in id|embedded_media:0 %}
    {% media media_id width=315 extent %}
{% endfor %}
```

You can also fetch all media ids embedded in a text:


```django
{% for media_id in id.body|embedded_media %}
    {% media media_id width=315 extent %}
{% endfor %}
```
").
-export([
    embedded_media/2,
    embedded_media/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


embedded_media(Id, Context) ->
    embedded_media(Id, true, Context).

embedded_media(undefined, _IsCheckBlocks, _Context) ->
    [];
embedded_media(#trans{} = Tr, _IsCheckBlocks, Context) ->
    embedded_media_1(Tr, Context);
embedded_media(Text, _IsCheckBlocks, Context) when is_binary(Text) ->
    embedded_media_1(Text, Context);
embedded_media(Id, IsCheckBlocks0, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined -> [];
        RscId ->
            IsCheckBlocks = z_convert:to_bool(IsCheckBlocks0),
            MediaIds = embedded_media_1(m_rsc:p(RscId, <<"body">>, Context), Context)
                ++ embedded_media_1(m_rsc:p(RscId, <<"body_extra">>, Context), Context)
                ++ embedded_media_blocks_1(IsCheckBlocks, m_rsc:p(RscId, <<"blocks">>, Context), Context),
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
            fun
                (#{ <<"body">> := BlockBody }) ->
                    embedded_media_1(BlockBody, Context);
                (#{ <<"body_extra">> := BlockBody }) ->
                    embedded_media_1(BlockBody, Context);
                (_) ->
                    []
            end,
            Blocks));
embedded_media_blocks_1(true, _Blocks, _Context) ->
    [].

embedded_media_1(undefined, _Context) ->
    [];
embedded_media_1(<<>>, _Context) ->
    [];
embedded_media_1(#trans{ tr = Tr }, Context) ->
    Found = lists:map(
        fun({_, Text}) ->
            embedded_media_1(Text, Context)
        end,
        Tr),
    lists:flatten(Found);
embedded_media_1(Text, _Context) when is_binary(Text) ->
    case re:run(Text, "\\<\\!-- z-media ([0-9]+) ", [global, {capture, all_but_first, binary}]) of
        nomatch -> [];
        {match, L} -> [binary_to_integer(I) || [I] <- L]
    end;
embedded_media_1(Text, Context) ->
    embedded_media_1(z_convert:to_binary(Text), Context).

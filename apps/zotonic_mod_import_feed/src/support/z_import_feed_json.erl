%% @copyright 2026 Marc Worrell
%% @doc Normalize JSON Feed 1/1.1 into the shared feed import representation.
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

-module(z_import_feed_json).

-export([parse/3]).

-spec parse(Data, Url, Limit) -> {ok, map()} | {error, term()} when
    Data :: binary(), Url :: binary(), Limit :: pos_integer().
parse(Data, Url, Limit) ->
    try
        z_json:decode(Data)
    of
        #{<<"version">> := Version, <<"title">> := Title, <<"items">> := Items} = Feed
                when is_binary(Title), is_list(Items),
                     (Version =:= <<"https://jsonfeed.org/version/1">> orelse
                      Version =:= <<"https://jsonfeed.org/version/1.1">>) ->
            normalize(Feed, Items, Url, Limit);
        _ ->
            {error, invalid_feed}
    catch _:_ ->
        {error, invalid_feed}
    end.

normalize(Feed, Items, Url, Limit) ->
    Entries = [entry(Item, Feed, Url) || Item <- lists:sublist(Items, Limit), valid_item(Item)],
    IsTruncated = length(Items) > Limit,
    IsZotonic = case lists:any(fun(E) -> is_binary(maps:get(zotonic_export, E)) end, Entries) of
        true -> true;
        false when Entries =:= []; IsTruncated -> undefined;
        false -> false
    end,
    {ok, #{
        url => Url,
        title => string_value(<<"title">>, Feed),
        website => resolve(string_value(<<"home_page_url">>, Feed), Url),
        hubs => [HubUrl || #{<<"type">> := <<"WebSub">>, <<"url">> := Href} <- list_value(<<"hubs">>, Feed),
                          HubUrl <- [resolve(Href, Url)], HubUrl =/= <<>>],
        entries => Entries,
        is_zotonic_feed => IsZotonic,
        refresh_seconds => 0,
        truncated => IsTruncated
    }}.

valid_item(#{<<"id">> := Id} = Item) when is_binary(Id), Id =/= <<>> ->
    is_binary(maps:get(<<"content_html">>, Item, undefined))
    orelse is_binary(maps:get(<<"content_text">>, Item, undefined));
valid_item(_) ->
    false.

entry(Item, Feed, FeedUrl) ->
    Guid = string_value(<<"id">>, Item),
    Link = resolve(string_value(<<"url">>, Item), FeedUrl),
    Base = case Link of <<>> -> FeedUrl; _ -> Link end,
    Body = case maps:get(<<"content_html">>, Item, undefined) of
        Html when is_binary(Html) -> Html;
        _ -> z_html:escape(string_value(<<"content_text">>, Item))
    end,
    {Images, Aliases} = z_import_feed_parse:image_variants(Body, Base),
    Media = [resolve(Href, Base) || #{<<"url">> := Href} <- list_value(<<"attachments">>, Item)]
         ++ [resolve(string_value(K, Item), Base) || K <- [<<"image">>, <<"banner_image">>]]
         ++ Images,
    #{
        uri => z_import_feed_parse:identity(Guid, Link, FeedUrl),
        guid => Guid,
        url => Link,
        % The shared importer unescapes titles and strips HTML from summaries.
        title => z_html:escape(string_value(<<"title">>, Item)),
        summary => z_html:escape(string_value(<<"summary">>, Item)),
        body => Body,
        author => author(Item, Feed),
        published => z_import_feed_parse:date(string_value(<<"date_published">>, Item)),
        modified => z_import_feed_parse:date(string_value(<<"date_modified">>, Item)),
        tags => lists:usort([Tag || Tag <- list_value(<<"tags">>, Item), is_binary(Tag), Tag =/= <<>>]),
        media => unique([maps:get(U, Aliases, U) || U <- Media, U =/= <<>>]),
        media_aliases => Aliases,
        zotonic_export => native(Item, Base),
        raw => Item
    }.

%% Only the explicit extension opts into the native resource import pipeline.
native(#{<<"_zotonic">> := #{<<"resource_url">> := Href}}, Base) ->
    case resolve(Href, Base) of
        <<>> -> undefined;
        Url -> Url
    end;
native(_, _) ->
    undefined.

author(Item, Feed) ->
    case authors(Item) of
        [] -> first_author(authors(Feed));
        Authors -> first_author(Authors)
    end.

authors(Map) ->
    case list_value(<<"authors">>, Map) of
        [] ->
            case maps:get(<<"author">>, Map, undefined) of
                Author when is_map(Author) -> [Author];
                _ -> []
            end;
        Authors ->
            Authors
    end.

first_author(Authors) ->
    case [Name || #{<<"name">> := Name} <- Authors, is_binary(Name), Name =/= <<>>] of
        [Name | _] -> Name;
        [] -> <<>>
    end.

string_value(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        Value when is_binary(Value) -> Value;
        _ -> <<>>
    end.

list_value(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        Value when is_list(Value) -> Value;
        _ -> []
    end.

resolve(Url, Base) ->
    z_import_feed_http:resolve(Url, Base).

unique(Items) ->
    {_, Unique} = lists:foldl(
        fun(Item, {Seen, Acc}) ->
            case maps:is_key(Item, Seen) of
                true -> {Seen, Acc};
                false -> {Seen#{Item => true}, [Item | Acc]}
            end
        end,
        {#{}, []},
        Items),
    lists:reverse(Unique).

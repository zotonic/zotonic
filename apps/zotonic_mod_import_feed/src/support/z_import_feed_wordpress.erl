%% @copyright 2026 Marc Worrell
%% @doc Optional WordPress REST enrichment; RSS remains usable if REST is disabled.

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

-module(z_import_feed_wordpress).
-define(MAX_MEDIA_ITEMS, 20).
-export([enrich/2, endpoint/1]).

-spec enrich(Entry, Context) -> map() when Entry :: map(), Context :: z:context().
enrich(#{zotonic_export := Url} = Entry, _) when is_binary(Url) -> Entry;
enrich(Entry, Context) ->
    case find_endpoint(Entry, Context) of
        undefined -> Entry;
        Url ->
            case z_import_feed_http:get(Url, Context) of
                {ok, {_, _, _, Body}} ->
                    try z_json:decode(Body) of
                        #{<<"id">> := _, <<"content">> := #{<<"rendered">> := Content}} = Post ->
                            merge(Entry, Post, Content);
                        _ -> Entry#{wordpress_error => invalid_response}
                    catch _:_ -> Entry#{wordpress_error => invalid_json} end;
                {error, Reason} -> Entry#{wordpress_error => Reason}
            end
    end.

find_endpoint(Entry, Context) ->
    case endpoint(maps:get(guid, Entry, <<>>)) of
        undefined ->
            case maps:get(url, Entry, <<>>) of
                <<>> -> undefined;
                Url -> discover_endpoint(Url, Context)
            end;
        Url -> Url
    end.

discover_endpoint(Url, Context) ->
    case z_import_feed_http:get(Url, Context) of
        {ok, {Final, Headers, _, Html}} ->
            {ok, Metadata} = z_url_metadata:fetch_data(Final, Headers, Html),
            Links = maps:get(<<"alternate">>, z_url_metadata:p(links, Metadata), []),
            Posts = [PostUrl || #{<<"type">> := <<"application/json">>, <<"href">> := Href} <- Links,
                PostUrl <- [z_import_feed_http:resolve(Href, z_convert:to_binary(Final))],
                binary:match(PostUrl, <<"wp/v2/posts/">>) =/= nomatch, z_import_feed_http:url(PostUrl)],
            case Posts of
                [Api | _] ->
                    Parts = uri_string:parse(Api),
                    Query = uri_string:dissect_query(maps:get(query, Parts, <<>>)),
                    uri_string:recompose(Parts#{query => uri_string:compose_query(
                        [{<<"_embed">>, <<"1">>} | proplists:delete(<<"_embed">>, Query)])});
                [] -> undefined
            end;
        _ -> undefined
    end.

%% WordPress RSS GUIDs identify the post independently of its mutable slug.
%% Retain the installation path for WordPress installations in a subdirectory.
-spec endpoint(Guid) -> binary() | undefined when Guid :: binary().
endpoint(Guid) ->
    try uri_string:parse(Guid) of
        #{scheme := _, host := _, query := Query} = Parts ->
            case proplists:get_value(<<"p">>, uri_string:dissect_query(Query)) of
                Id when is_binary(Id), byte_size(Id) > 0 ->
                    case re:run(Id, <<"^[0-9]+$">>, [{capture, none}]) of
                        match ->
                            Path = maps:get(path, Parts, <<"/">>),
                            Root = filename:dirname(binary_to_list(Path)),
                            %% dirname('/') is '/', dirname('/blog/') is '/blog'.
                            ApiPath = iolist_to_binary([string:trim(Root, trailing, "/"),
                                "/wp-json/wp/v2/posts/", Id]),
                            case uri_string:recompose((maps:without([fragment, userinfo], Parts))#{
                                path => ApiPath, query => <<"_embed=1">>}) of
                                Url when is_binary(Url) -> Url;
                                _ -> undefined
                            end;
                        _ -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    catch _:_ -> undefined end.

merge(Entry, Post, Content) ->
    Embedded = maps:get(<<"_embedded">>, Post, #{}),
    Authors = maps:get(<<"author">>, Embedded, []),
    Media = maps:get(<<"wp:featuredmedia">>, Embedded, []),
    Author = case Authors of [#{<<"name">> := Name} | _] -> Name; _ -> maps:get(author, Entry) end,
    Featured = [MediaUrl || #{<<"source_url">> := MediaUrl} <- Media, z_import_feed_http:url(MediaUrl)],
    {Images, BodyAliases} = z_import_feed_parse:image_variants(
        <<(maps:get(body, Entry))/binary, Content/binary>>, maps:get(url, Entry)),
    FeaturedAliases = lists:foldl(fun featured_aliases/2, #{}, Media),
    Aliases = maps:merge(maps:map(fun(_, Selected) ->
        maps:get(Selected, FeaturedAliases, Selected)
    end, BodyAliases), FeaturedAliases),
    OtherMedia = lists:usort([maps:get(MediaUrl, Aliases, MediaUrl) || MediaUrl <- maps:get(media, Entry) ++ Images, MediaUrl =/= <<>>]),
    Published = utc(maps:get(<<"date_gmt">>, Post, <<>>)),
    Modified = utc(maps:get(<<"modified_gmt">>, Post, <<>>)),
    Entry#{body => Content, author => Author,
        published => fallback(Published, maps:get(published, Entry)),
        modified => fallback(Modified, maps:get(modified, Entry)),
        media => lists:sublist(Featured ++ [MediaUrl || MediaUrl <- OtherMedia, not lists:member(MediaUrl, Featured)], ?MAX_MEDIA_ITEMS),
        media_aliases => Aliases,
        raw => #{feed => maps:get(raw, Entry), wordpress => Post}}.

featured_aliases(#{<<"source_url">> := Original} = Media, Acc) ->
    Details = maps:get(<<"media_details">>, Media, #{}),
    Sizes = maps:values(maps:get(<<"sizes">>, Details, #{})),
    lists:foldl(fun
        (#{<<"source_url">> := SizeUrl}, AliasAcc) -> AliasAcc#{SizeUrl => Original};
        (_, AliasAcc) -> AliasAcc
    end, Acc#{Original => Original}, Sizes);
featured_aliases(_, Acc) -> Acc.

utc(<<>>) -> undefined;
utc(DateText) -> z_import_feed_parse:date(<<DateText/binary, "Z">>).
fallback(undefined, Default) -> Default;
fallback(Value, _) -> Value.

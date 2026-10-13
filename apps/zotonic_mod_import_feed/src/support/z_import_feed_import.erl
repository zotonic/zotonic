%% @copyright 2026 Marc Worrell
%% @doc Normalize, notify and import one entry. Identity locks and mapping writes
%% share the resource transaction, so retrying an interrupted import is safe.

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

-module(z_import_feed_import).
-define(MAX_MEDIA_ITEMS, 20).
-define(MAX_MEDIA_BYTES, 20 * 1024 * 1024).
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../../include/mod_import_feed.hrl").
-export([prepare/3, run/3, run/4, local_body/3, normalize_links/3]).

-spec prepare(map(), map(), z:context()) -> {map(), [map()]}.
prepare(E, F, C) ->
    {ok, Patterns} = z_import_feed_cleanup:compile(maps:get(remove_patterns, F, <<>>)),
    Cat = category(E, F, C),
    Common = (copyright_props(F, C))#{<<"is_published">> => true,
        <<"is_authoritative">> => maps:get(is_authoritative, F),
        <<"content_group_id">> => maps:get(content_group_id, F)},
    Props = maps:filter(fun(_, V) -> V =/= undefined end, Common#{
        <<"category_id">> => Cat, <<"title">> => z_html:unescape(maps:get(title, E)),
        <<"body">> => z_html:abs_links(z_import_feed_cleanup:html(maps:get(body, E), Patterns), maps:get(url, E)),
        <<"summary">> => z_import_feed_cleanup:text(z_html:unescape(z_html:strip(maps:get(summary, E))), Patterns),
        <<"website">> => maps:get(url, E), <<"author">> => maps:get(author, E),
        <<"publication_start">> => maps:get(published, E),
        <<"feed_modified">> => maps:get(modified, E)}),
    Media = lists:sublist([U || U <- maps:get(media, E), U =/= maps:get(uri, E)], ?MAX_MEDIA_ITEMS),
    {Medium, Dependents} = case {maps:get(media_mode, F), Media} of
        {<<"attached">>, [U | _]} -> {U, []};
        {<<"dependent">>, _} -> {undefined, [#{uri => U, medium => U,
            props => Common#{<<"category_id">> => m_rsc:rid(media, C),
                <<"title">> => z_html:unescape(maps:get(title, E)), <<"is_dependent">> => true}}
            || U <- Media]};
        _ -> {undefined, []}
    end,
    Edges = maps:get(connections, F) ++ [{m_rsc:rid(depiction, C), maps:get(uri, D)} || D <- Dependents],
    Item = #{uri => maps:get(uri, E), props => Props, edges => Edges, medium => Medium,
        media_aliases => maps:get(media_aliases, E, #{})},
    z_import_feed_native:prepare(E, Item, Dependents, F, C).

%% Defaults apply to the article and its dependent media before observers run.
%% Empty defaults leave resource-level fallback behavior to mod_copyright.
copyright_props(F, C) ->
    case z_module_manager:active(mod_copyright, C) of
        false -> #{};
        true -> maps:from_list([
            {atom_to_binary(Key, utf8), Value}
            || Key <- [rights, rights_attribution, rights_year],
               Value <- [maps:get(Key, F, undefined)],
               is_binary(Value), Value =/= <<>>
        ])
    end.

-spec run(map(), map(), z:context()) -> imported | updated | skipped | {error, term()}.
run(E, F, C) ->
    run(E, F, false, C).

%% @doc Explicit refresh bypasses the checksum, while retaining deletion tombstones.
-spec run(map(), map(), boolean(), z:context()) -> imported | updated | skipped | {error, term()}.
run(#{uri := undefined}, _, _, _) -> {error, missing_identity};
run(E, F, Force, C) ->
    {Item, Ds} = prepare(E, F, C),
    Uri = maps:get(uri, Item),
    Hash = crypto:hash(sha256, term_to_binary({Item, Ds})),
    FeedId = maps:get(id, F),
    Result = z_db:transaction(fun(Tx) ->
        %% A site-wide transaction lock also orders article/dependent locks.
        %% It prevents deadlocks for two feeds importing shared media in reverse order.
        z_db:q("select pg_advisory_xact_lock(hashtext(current_schema()), 194815)", Tx),
        case z_db:q1("select checksum from import_feed_item where feed_id=$1 and uri=$2 and status='ok'",
                [FeedId, Uri], Tx) of
            Hash when not Force -> skipped;
            _ ->
                case deleted(Uri, Tx) of
                    true -> record(FeedId, Uri, <<"article">>, <<"deleted">>, Hash, undefined, Tx), skipped;
                    false ->
                        Notice = #import_feed_item{item = Item, dependents = Ds,
                            feed = maps:without([user_groups, lease_token], F), raw = maps:get(raw, E)},
                        Existing = lookup(Uri, Tx),
                        case z_notifier:first(Notice, Tx) of
                            undefined ->
                                {Id, Imported} = import(Item, Ds, Tx),
                                finish(Id, Imported, Existing, Uri, FeedId, Hash, Tx);
                            {ok, Id} -> finish(Id, #{}, Existing, Uri, FeedId, Hash, Tx);
                            {ok, Id, Imported} when is_map(Imported) ->
                                finish(Id, Imported, Existing, Uri, FeedId, Hash, Tx);
                            {skip, Reason} ->
                                record(FeedId, Uri, <<"article">>, <<"skipped">>, Hash, Reason, Tx), skipped;
                            {error, Reason} -> throw({import_error, Reason});
                            _ -> throw({import_error, invalid_observer_result})
                        end
                end
        end
    end, C),
    case Result of
        {rollback, Reason} ->
            record(FeedId, Uri, <<"article">>, <<"error">>, undefined, Reason, C),
            {error, Reason};
        Other -> Other
    end.

finish(Id, Imported, Existing, Uri, FeedId, Hash, C) ->
    true = is_integer(Id) andalso z_acl:rsc_editable(Id, C),
    remember(Uri, Id, C),
    maps:foreach(fun(U, RId) ->
        true = is_integer(RId) andalso z_acl:rsc_visible(RId, C),
        remember(U, RId, C),
        Kind = case m_rsc:is_a(RId, media, C) of true -> <<"media">>; false -> <<"resource">> end,
        record(FeedId, U, Kind, <<"ok">>, undefined, undefined, C)
    end, Imported),
    record(FeedId, Uri, <<"article">>, <<"ok">>, Hash, undefined, C),
    case Existing of undefined -> imported; _ -> updated end.

import(#{zotonic_export := _} = Item, _, Context) ->
    {Id, Imported} = z_import_feed_native:run(Item, lookup(maps:get(uri, Item), Context), Context),
    lists:foreach(fun({P, O}) ->
                          edge(Id, P, O, Context)
                  end,
                  maps:get(default_connections, Item)),
    {Id, Imported};
import(Item, Ds, Context) ->
    %% Reuse media by source URI. Do not steal media from existing local resources.
    Imported = lists:foldl(fun(D, Acc) ->
                                   Uri = maps:get(uri, D),
                                   case deleted(Uri, Context) of
                                       true -> Acc;
                                       false ->
                                           Id = upsert(D, Context),
                                           ok = medium(Id, maps:get(medium, D), Context),
                                           remember(Uri, Id, Context),
                                           Acc#{Uri => Id}
                                   end
                           end,
                           #{},
                           Ds),
    Props = local_body(maps:get(props, Item), media_aliases(Item, Imported), Context),
    Item1 = Item#{props := Props},
    Id = upsert(Item1, Context),
    ok = medium(Id, maps:get(medium, Item), Context),
    case maps:get(medium, Item) of
        undefined ->
            ok;
        Attached ->
            LocalProps = local_body(maps:get(props, Item), media_aliases(Item, #{Attached => Id}), Context),
            case m_rsc:update(Id, maps:with([<<"body">>], LocalProps), Context) of
                {ok, _} -> ok;
                {error, Reason} -> throw({import_error, Reason})
            end
    end,
    lists:foreach(fun ({P, O}) when is_integer(O) ->
                          edge(Id, P, O, Context);
                      ({P, U}) ->
                          case maps:find(U, Imported) of
                              {ok, O} ->
                                  edge(Id, P, O, Context);
                              error ->
                                  ok
                          end
                  end,
                  maps:get(edges, Item)),
    {Id, Imported}.

media_aliases(Item, Imported) ->
    maps:fold(fun(Alias, Selected, Acc) ->
        case maps:find(Selected, Imported) of
            {ok, Id} -> Acc#{Alias => Id};
            error -> Acc
        end
    end, Imported, maps:get(media_aliases, Item, #{})).

%% @doc Replace imported inline images with editable Zotonic media embeds.
%% The map includes source variant aliases, each pointing to a local resource.
-spec local_body(Props, Imported, Context) -> map() when
    Props :: map(), Imported :: #{binary() => integer()}, Context :: z:context().
local_body(#{<<"body">> := Body} = Props, Imported, Context) ->
    Urls = maps:fold(fun(U, Id, Acc) ->
                             case z_media_tag:url(Id, [], Context) of
                                 {ok, Local} when Local =/= <<>> ->
                                     Acc#{U => Local};
                                 _ ->
                                     Acc
                             end
                     end, #{}, Imported),
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            Html = iolist_to_binary([body_html(local_node(N, Imported, Urls, maps:get(<<"website">>, Props, <<>>))) || N <- Children]),
            Props#{<<"body">> := normalize_links(Html, maps:get(<<"website">>, Props, <<>>), Context)};
        _ ->
            Props
    end;
local_body(Props, _, _) ->
    Props.

%% Fragment children can be plain text, including whitespace between paragraphs.
body_html(Text) when is_binary(Text) -> z_html_parse:escape(Text);
body_html(Node) -> z_html_parse:to_html(Node).

%% WordPress wraps images in links to the original medium. Use a single embed
%% for these wrappers, also allowing repair of previously stored placeholders.
local_node({<<"a">>, Attrs, [{<<"img">>, _, _}]} = Node, Imported, Urls, Base) ->
    case maps:get(proplists:get_value(<<"href">>, Attrs), Imported, undefined) of
        Id when is_integer(Id) -> media_embed(Id);
        undefined -> local_element(Node, Imported, Urls, Base)
    end;
local_node({<<"img">>, _, _} = Node, Imported, Urls, Base) ->
    [Selected | _] = z_import_feed_parse:images(
        iolist_to_binary(z_html_parse:to_html(Node)), Base) ++ [undefined],
    case maps:get(Selected, Imported, undefined) of
        Id when is_integer(Id) -> media_embed(Id);
        undefined -> local_element(Node, Imported, Urls, Base)
    end;
local_node({_, _, _} = Node, Imported, Urls, Base) ->
    local_element(Node, Imported, Urls, Base);
local_node(Node, _, _, _) -> Node.

media_embed(Id) ->
    {comment, iolist_to_binary([" z-media ", integer_to_binary(Id),
        " {\"size\":\"large\",\"align\":\"block\"} "])}.

local_element({Tag, Attrs, Children}, Imported, Urls, Base) ->
    Attrs1 = [{K, case lists:member(K, [<<"src">>, <<"href">>, <<"poster">>]) of
                    true -> maps:get(V, Urls, V); false -> V end}
        || {K, V} <- Attrs],
    {Tag, Attrs1, [local_node(N, Imported, Urls, Base) || N <- Children]}.

%% @doc Remove image hyperlinks and resolve article links against the source.
%% Only resources recorded as imported articles can become local destinations.
-spec normalize_links(Body, Base, Context) -> binary() when
    Body :: binary(), Base :: binary(), Context :: z:context().
normalize_links(Body, Base, Context) ->
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            iolist_to_binary([body_html(N) || N <- link_nodes(Children, Base, Context)]);
        _ -> Body
    end.

link_nodes(Nodes, Base, Context) ->
    lists:flatmap(fun
        ({<<"a">>, Attrs, Children}) ->
            Href = proplists:get_value(<<"href">>, Attrs, <<>>),
            Resolved = z_import_feed_http:resolve(Href, Base),
            Content = link_nodes(Children, Base, Context),
            case image_link(Resolved) of
                true -> Content;
                false ->
                    Link = case Resolved of
                        <<>> -> Href;
                        _ -> article_link(Resolved, Context)
                    end,
                    [{<<"a">>, lists:keystore(<<"href">>, 1, Attrs, {<<"href">>, Link}), Content}]
            end;
        ({Tag, Attrs, Children}) -> [{Tag, Attrs, link_nodes(Children, Base, Context)}];
        (Node) -> [Node]
    end, Nodes).

image_link(<<>>) -> false;
image_link(Url) ->
    Path = maps:get(path, uri_string:parse(Url), <<>>),
    re:run(Path, <<"\\.(?:jpe?g|png|gif|webp|avif|svg|tiff?|bmp|ico)$">>,
        [caseless, {capture, none}]) =:= match.

article_link(Url, Context) ->
    Parts = uri_string:parse(Url),
    Source = uri_string:recompose(maps:remove(fragment, Parts)),
    Id = imported_article(Source, Context),
    case is_integer(Id) andalso z_acl:rsc_visible(Id, Context) of
        true ->
            case m_rsc:p(Id, page_url, Context) of
                undefined -> Url;
                Page ->
                    Fragment = case maps:get(fragment, Parts, undefined) of
                        undefined -> <<>>;
                        F -> <<"#", F/binary>>
                    end,
                    iolist_to_binary([Page, Fragment])
            end;
        false -> Url
    end.

imported_article(Source, Context) ->
    case z_db:q1("select f.rsc_id from import_feed_resource f
            join import_feed_item i on i.uri=f.uri
            where f.uri=$1 and i.kind='article' and f.rsc_id is not null
            limit 1", [Source], Context) of
        undefined ->
            %% Resource properties can be stored as either term or JSON data.
            %% Read website via the model rather than depending on that storage.
            Ids = z_db:q("select distinct f.rsc_id from import_feed_resource f
                join import_feed_item i on i.uri=f.uri
                where i.kind='article' and f.rsc_id is not null", Context),
            find_article_website(Source, Ids, Context);
        Id -> Id
    end.

find_article_website(_, [], _) -> undefined;
find_article_website(Source, [{Id} | Rest], Context) ->
    case z_html:unescape(m_rsc:p(Id, website, Context)) of
        Source -> Id;
        _ -> find_article_website(Source, Rest, Context)
    end.

edge(Id, P, O, Context) ->
    case m_edge:insert(Id, P, O, Context) of
        {ok, _} -> ok;
        {error, R} -> throw({import_error, {edge, R}})
    end.

upsert(#{uri := Uri, props := P}, Context) ->
    Authority = maps:get(<<"is_authoritative">>, P, false),
    Props = P#{<<"uri">> => case Authority of
                                true -> undefined;
                                false -> Uri
                            end,
               <<"feed_source_uri">> => Uri},
    Result = case lookup(Uri, Context) of
                 undefined ->
                     m_rsc:insert(Props, [is_import, {tz, <<"UTC">>}], Context);
                 Id ->
                     %% Only a resource already mapped by this importer may be authoritative.
                     Owned = z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context) =:= Id,
                     case Owned orelse not m_rsc:p(Id, is_authoritative, Context) of
                         true ->
                             UpdateProps = case maps:get(<<"is_dependent">>, P, false) of
                                               true ->
                                                   case m_rsc:is_a(Id, media, Context) of
                                                       true -> maps:remove(<<"category_id">>, Props);
                                                       false -> throw({import_error, media_identity_conflict})
                                                   end;
                                               false -> Props
                                           end,
                             m_rsc:update(Id, UpdateProps, [is_import, {tz, <<"UTC">>}], Context);
                         false ->
                             {error, authoritative_resource}
                     end
             end,
    case Result of
        {ok, Id1} ->
            remember(Uri, Id1, Context), Id1;
        {error, R} ->
            throw({import_error, R})
    end.

medium(_, undefined, _) -> ok;
medium(Id, Url, Context) ->
    case m_rsc:p(Id, feed_medium_url, Context) of
        Url ->
            ok;
        _ ->
            case z_import_feed_http:get(Url, ?MAX_MEDIA_BYTES, Context) of
                {ok, {Final, _, _, Data}} ->
                    #{path := Path} = uri_string:parse(z_convert:to_binary(Final)),
                    Upload = #upload{filename = filename:basename(Path), data = Data},
                    case m_media:replace_file(Upload, Id, #{<<"feed_medium_url">> => Url}, Context) of
                        {ok, _} ->
                            ok;
                        {error, R} ->
                            throw({import_error, {media, R}})
                    end;
                {error, R} ->
                    throw({import_error, {media_fetch, R}})
            end
    end.

lookup(Uri, C) ->
    case z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], C) of
        undefined -> m_rsc:rid(Uri, C);
        Id -> Id
    end.
deleted(Uri, C) ->
    z_db:q1("select count(*) from import_feed_resource where uri=$1 and rsc_id is null", [Uri], C) =:= 1.
remember(Uri, Id, C) ->
    z_db:q("insert into import_feed_resource(uri,rsc_id) values($1,$2)
        on conflict(uri) do update set rsc_id=excluded.rsc_id", [Uri, Id], C), ok.
record(FeedId, Uri, Kind, Status, Hash, Error, C) ->
    Msg = case Error of undefined -> undefined; _ -> iolist_to_binary(io_lib:format("~0P", [Error, 8])) end,
    z_db:q("insert into import_feed_item(feed_id,uri,kind,status,checksum,last_error)
        values($1,$2,$3,$4,$5,$6) on conflict(feed_id,uri) do update
        set status=excluded.status, checksum=excluded.checksum, last_error=excluded.last_error, modified=now()",
        [FeedId, Uri, Kind, Status, Hash, Msg], C), ok.

category(E, #{guess_category := true, category_id := Default}, C) ->
    Tags = [z_string:to_lower(T) || T <- maps:get(tags, E)],
    Candidates = [Id || Cat <- m_category:tree_flat(C),
        Id <- [proplists:get_value(id, Cat)], is_integer(Id),
        not m_category:is_a(Id, meta, C),
        lists:any(fun(T) -> lists:member(T, Tags) end,
            [z_string:to_lower(z_convert:to_binary(m_rsc:p(Id, name, C))),
             z_string:to_lower(z_trans:lookup_fallback(m_rsc:p(Id, title, C), C))])],
    case Candidates of
        [Id | _] -> Id;
        [] ->
            %% The configured category might have moved under meta since saving.
            case m_category:is_a(Default, meta, C) of
                true -> m_rsc:rid(article, C);
                false -> Default
            end
    end;
category(_, F, _) -> maps:get(category_id, F).

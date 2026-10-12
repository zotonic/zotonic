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
prepare(Entry, FeedSettings, Context) ->
    {ok, Patterns} = z_import_feed_cleanup:compile(maps:get(remove_patterns, FeedSettings, <<>>)),
    Category = category(Entry, FeedSettings, Context),
    Common = (copyright_props(FeedSettings, Context))#{<<"is_published">> => true,
        <<"is_authoritative">> => maps:get(is_authoritative, FeedSettings),
        <<"content_group_id">> => maps:get(content_group_id, FeedSettings)},
    Props = maps:filter(fun(_, Value) -> Value =/= undefined end, Common#{
        <<"category_id">> => Category, <<"title">> => z_html:unescape(maps:get(title, Entry)),
        <<"body">> => z_html:abs_links(z_import_feed_cleanup:html(maps:get(body, Entry), Patterns), maps:get(url, Entry)),
        <<"summary">> => z_import_feed_cleanup:text(z_html:unescape(z_html:strip(maps:get(summary, Entry))), Patterns),
        <<"website">> => maps:get(url, Entry), <<"author">> => maps:get(author, Entry),
        <<"publication_start">> => maps:get(published, Entry),
        <<"feed_modified">> => maps:get(modified, Entry)}),
    Media = lists:sublist([MediaUrl || MediaUrl <- maps:get(media, Entry), MediaUrl =/= maps:get(uri, Entry)], ?MAX_MEDIA_ITEMS),
    {Medium, Dependents} = case {maps:get(media_mode, FeedSettings), Media} of
        {<<"attached">>, [MediaUrl | _]} -> {MediaUrl, []};
        {<<"dependent">>, _} -> {undefined, [#{uri => MediaUrl, medium => MediaUrl,
            props => Common#{<<"category_id">> => m_rsc:rid(media, Context),
                <<"title">> => z_html:unescape(maps:get(title, Entry)), <<"is_dependent">> => true}}
            || MediaUrl <- Media]};
        _ -> {undefined, []}
    end,
    Edges = maps:get(connections, FeedSettings) ++ [{m_rsc:rid(depiction, Context), maps:get(uri, Dependent)} || Dependent <- Dependents],
    Item = #{uri => maps:get(uri, Entry), props => Props, edges => Edges, medium => Medium,
        media_aliases => maps:get(media_aliases, Entry, #{})},
    z_import_feed_native:prepare(Entry, Item, Dependents, FeedSettings, Context).

%% Defaults apply to the article and its dependent media before observers run.
%% Empty defaults leave resource-level fallback behavior to mod_copyright.
copyright_props(FeedSettings, Context) ->
    case z_module_manager:active(mod_copyright, Context) of
        false -> #{};
        true -> maps:from_list([
            {atom_to_binary(Key, utf8), Value}
            || Key <- [rights, rights_attribution, rights_year],
               Value <- [maps:get(Key, FeedSettings, undefined)],
               is_binary(Value), Value =/= <<>>
        ])
    end.

-spec run(map(), map(), z:context()) -> imported | updated | skipped | {error, term()}.
run(Entry, FeedSettings, Context) ->
    run(Entry, FeedSettings, false, Context).

%% @doc Explicit refresh bypasses the checksum, while retaining deletion tombstones.
-spec run(map(), map(), boolean(), z:context()) -> imported | updated | skipped | {error, term()}.
run(#{uri := undefined}, _, _, _) -> {error, missing_identity};
run(Entry, FeedSettings, Force, Context) ->
    {Item, Dependents} = prepare(Entry, FeedSettings, Context),
    Uri = maps:get(uri, Item),
    Hash = crypto:hash(sha256, term_to_binary({Item, Dependents})),
    FeedId = maps:get(id, FeedSettings),
    Result = z_db:transaction(fun(Ctx) ->
        %% A site-wide transaction lock also orders article/dependent locks.
        %% It prevents deadlocks for two feeds importing shared media in reverse order.
        z_db:q("select pg_advisory_xact_lock(hashtext(current_schema()), 194815)", Ctx),
        case z_db:q1("select checksum from import_feed_item where feed_id=$1 and uri=$2 and status='ok'",
                [FeedId, Uri], Ctx) of
            Hash when not Force -> skipped;
            _ ->
                case deleted(Uri, Ctx) of
                    true -> record(FeedId, Uri, <<"article">>, <<"deleted">>, Hash, undefined, Ctx), skipped;
                    false ->
                        Notice = #import_feed_item{item = Item, dependents = Dependents,
                            feed = maps:without([user_groups, lease_token], FeedSettings), raw = maps:get(raw, Entry)},
                        Existing = lookup(Uri, Ctx),
                        case z_notifier:first(Notice, Ctx) of
                            undefined ->
                                {Id, Imported} = import(Item, Dependents, Ctx),
                                finish(Id, Imported, Existing, Uri, FeedId, Hash, Ctx);
                            {ok, Id} -> finish(Id, #{}, Existing, Uri, FeedId, Hash, Ctx);
                            {ok, Id, Imported} when is_map(Imported) ->
                                finish(Id, Imported, Existing, Uri, FeedId, Hash, Ctx);
                            {skip, Reason} ->
                                record(FeedId, Uri, <<"article">>, <<"skipped">>, Hash, Reason, Ctx), skipped;
                            {error, Reason} -> throw({import_error, Reason});
                            _ -> throw({import_error, invalid_observer_result})
                        end
                end
        end
    end, Context),
    case Result of
        {rollback, Reason} ->
            record(FeedId, Uri, <<"article">>, <<"error">>, undefined, Reason, Context),
            {error, Reason};
        Other -> Other
    end.

finish(Id, Imported, Existing, Uri, FeedId, Hash, Context) ->
    true = is_integer(Id) andalso z_acl:rsc_editable(Id, Context),
    remember(Uri, Id, Context),
    maps:foreach(fun(ImportedUri, RId) ->
        true = is_integer(RId) andalso z_acl:rsc_visible(RId, Context),
        remember(ImportedUri, RId, Context),
        Kind = case m_rsc:is_a(RId, media, Context) of true -> <<"media">>; false -> <<"resource">> end,
        record(FeedId, ImportedUri, Kind, <<"ok">>, undefined, undefined, Context)
    end, Imported),
    record(FeedId, Uri, <<"article">>, <<"ok">>, Hash, undefined, Context),
    case Existing of undefined -> imported; _ -> updated end.

import(#{zotonic_export := _} = Item, _, Context) ->
    {Id, Imported} = z_import_feed_native:run(Item, lookup(maps:get(uri, Item), Context), Context),
    lists:foreach(fun({Predicate, Object}) -> edge(Id, Predicate, Object, Context) end, maps:get(default_connections, Item)),
    {Id, Imported};
import(Item, Dependents, Context) ->
    %% Reuse media by source URI. Do not steal media from existing local resources.
    Imported = lists:foldl(fun(Dependent, Acc) ->
        DependentUri = maps:get(uri, Dependent),
        case deleted(DependentUri, Context) of
            true -> Acc;
            false ->
                Id = upsert(Dependent, Context),
                ok = medium(Id, maps:get(medium, Dependent), Context),
                remember(DependentUri, Id, Context),
                Acc#{DependentUri => Id}
        end
    end, #{}, Dependents),
    Item1 = Item#{props := local_body(maps:get(props, Item), media_aliases(Item, Imported), Context)},
    Id = upsert(Item1, Context),
    ok = medium(Id, maps:get(medium, Item), Context),
    case maps:get(medium, Item) of
        undefined -> ok;
        Attached ->
            LocalProps = local_body(maps:get(props, Item), media_aliases(Item, #{Attached => Id}), Context),
            case m_rsc:update(Id, maps:with([<<"body">>], LocalProps), Context) of
                {ok, _} -> ok;
                {error, Reason} -> throw({import_error, Reason})
            end
    end,
    lists:foreach(fun
        ({Predicate, Object}) when is_integer(Object) -> edge(Id, Predicate, Object, Context);
        ({Predicate, ObjectUri}) -> case maps:find(ObjectUri, Imported) of {ok, Object} -> edge(Id, Predicate, Object, Context); error -> ok end
    end, maps:get(edges, Item)),
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
    Urls = maps:fold(fun(SourceUrl, Id, Acc) ->
        case z_media_tag:url(Id, [], Context) of
            {ok, Local} when Local =/= <<>> -> Acc#{SourceUrl => Local};
            _ -> Acc
        end
    end, #{}, Imported),
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            Html = iolist_to_binary([body_html(local_node(Child, Imported, Urls, maps:get(<<"website">>, Props, <<>>))) || Child <- Children]),
            Props#{<<"body">> := normalize_links(Html, maps:get(<<"website">>, Props, <<>>), Context)};
        _ -> Props
    end;
local_body(Props, _, _) -> Props.

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
    Attrs1 = [{Key, case lists:member(Key, [<<"src">>, <<"href">>, <<"poster">>]) of
                    true -> maps:get(Value, Urls, Value); false -> Value end}
        || {Key, Value} <- Attrs],
    {Tag, Attrs1, [local_node(Child, Imported, Urls, Base) || Child <- Children]}.

%% @doc Remove image hyperlinks and resolve article links against the source.
%% Only resources recorded as imported articles can become local destinations.
-spec normalize_links(Body, Base, Context) -> binary() when
    Body :: binary(), Base :: binary(), Context :: z:context().
normalize_links(Body, Base, Context) ->
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            iolist_to_binary([body_html(Node) || Node <- link_nodes(Children, Base, Context)]);
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
                        Anchor -> <<"#", Anchor/binary>>
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

edge(Id, Predicate, Object, Context) ->
    case m_edge:insert(Id, Predicate, Object, Context) of
        {ok, _} -> ok;
        {error, Reason} -> throw({import_error, {edge, Reason}})
    end.

upsert(#{uri := Uri, props := ItemProps}, Context) ->
    Authority = maps:get(<<"is_authoritative">>, ItemProps, false),
    Props = ItemProps#{<<"uri">> => case Authority of
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
                             UpdateProps = case maps:get(<<"is_dependent">>, ItemProps, false) of
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
        {ok, ResourceId} ->
            remember(Uri, ResourceId, Context), ResourceId;
        {error, Reason} ->
            throw({import_error, Reason})
    end.

medium(_, undefined, _) -> ok;
medium(Id, Url, Context) ->
    case m_rsc:p(Id, feed_medium_url, Context) of
        Url -> ok;
        _ ->
            case z_import_feed_http:get(Url, ?MAX_MEDIA_BYTES, Context) of
                {ok, {Final, _, _, Data}} ->
                    #{path := Path} = uri_string:parse(z_convert:to_binary(Final)),
                    Upload = #upload{filename = filename:basename(Path), data = Data},
                    case m_media:replace_file(Upload, Id, #{<<"feed_medium_url">> => Url}, Context) of
                        {ok, _} -> ok;
                        {error, Reason} -> throw({import_error, {media, Reason}})
                    end;
                {error, Reason} -> throw({import_error, {media_fetch, Reason}})
            end
    end.

lookup(Uri, Context) ->
    case z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context) of
        undefined -> m_rsc:rid(Uri, Context);
        Id -> Id
    end.
deleted(Uri, Context) ->
    z_db:q1("select count(*) from import_feed_resource where uri=$1 and rsc_id is null", [Uri], Context) =:= 1.
remember(Uri, Id, Context) ->
    z_db:q("insert into import_feed_resource(uri,rsc_id) values($1,$2)
        on conflict(uri) do update set rsc_id=excluded.rsc_id", [Uri, Id], Context), ok.
record(FeedId, Uri, Kind, Status, Hash, Error, Context) ->
    Msg = case Error of undefined -> undefined; _ -> iolist_to_binary(io_lib:format("~0P", [Error, 8])) end,
    z_db:q("insert into import_feed_item(feed_id,uri,kind,status,checksum,last_error)
        values($1,$2,$3,$4,$5,$6) on conflict(feed_id,uri) do update
        set status=excluded.status, checksum=excluded.checksum, last_error=excluded.last_error, modified=now()",
        [FeedId, Uri, Kind, Status, Hash, Msg], Context), ok.

category(Entry, #{guess_category := true, category_id := Default}, Context) ->
    Tags = [z_string:to_lower(Tag) || Tag <- maps:get(tags, Entry)],
    Candidates = [Id || Category <- m_category:tree_flat(Context),
        Id <- [proplists:get_value(id, Category)], is_integer(Id),
        not m_category:is_a(Id, meta, Context),
        lists:any(fun(Tag) -> lists:member(Tag, Tags) end,
            [z_string:to_lower(z_convert:to_binary(m_rsc:p(Id, name, Context))),
             z_string:to_lower(z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context))])],
    case Candidates of
        [Id | _] -> Id;
        [] ->
            %% The configured category might have moved under meta since saving.
            case m_category:is_a(Default, meta, Context) of
                true -> m_rsc:rid(article, Context);
                false -> Default
            end
    end;
category(_, FeedSettings, _) -> maps:get(category_id, FeedSettings).

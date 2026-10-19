%% @copyright 2026 Marc Worrell
%% @doc Normalize, notify and import one entry. Source identity locks serialize
%% overlapping imports without holding a database transaction during downloads.
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

-module(z_import_feed_import).
-define(MAX_MEDIA_ITEMS, 20).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([prepare/3, run/3, run/4, local_body/3, normalize_links/3, track_resource/5]).

-spec prepare(map(), map(), z:context()) -> {map(), [map()]}.
prepare(Entry, FeedSettings, Context) ->
    {ok, Patterns} = z_import_feed_cleanup:compile(maps:get(remove_patterns, FeedSettings, <<>>)),
    CategoryId = category(Entry, FeedSettings, Context),
    Common = (copyright_props(FeedSettings, Context))#{
            <<"is_published">> => true,
            <<"is_authoritative">> => maps:get(is_authoritative, FeedSettings),
            <<"content_group_id">> => maps:get(content_group_id, FeedSettings)
        },
    Props = maps:filter(
        fun(_, Value) -> Value =/= undefined end,
        Common#{
            <<"category_id">> => CategoryId,
            <<"title">> => z_html:unescape(maps:get(title, Entry)),
            <<"body">> => z_html:abs_links(z_import_feed_cleanup:html(maps:get(body, Entry), Patterns), maps:get(url, Entry)),
            <<"summary">> => z_import_feed_cleanup:text(z_html:unescape(z_html:strip(maps:get(summary, Entry))), Patterns),
            <<"website">> => maps:get(url, Entry),
            <<"author">> => maps:get(author, Entry),
            <<"publication_start">> => maps:get(published, Entry),
            <<"feed_modified">> => maps:get(modified, Entry)
        }),
    Media = lists:sublist([MediaUri || MediaUri <- maps:get(media, Entry), MediaUri =/= maps:get(uri, Entry)], ?MAX_MEDIA_ITEMS),
    {Medium, Dependents} = case {maps:get(media_mode, FeedSettings), Media} of
                               {<<"attached">>, [MediaUri | _]} ->
                                   {MediaUri, []};
                               {<<"dependent">>, _} ->
                                   MediaItemsProps = [prepare_media_props(MediaUri, Entry, Common, Context) || MediaUri <- Media],
                                   {undefined, MediaItemsProps};
                               _ ->
                                   {undefined, []}
                           end,
    Edges = maps:get(connections, FeedSettings) ++ [{m_rsc:rid(depiction, Context), maps:get(uri, Dependent)} || Dependent <- Dependents],
    Item = #{
        uri => maps:get(uri, Entry),
        props => Props,
        edges => Edges,
        medium => Medium,
        media_aliases => maps:map(
            fun(_, Selected) -> z_import_feed_media:identity(Selected) end,
            maps:merge(maps:from_list([{Url, Url} || Url <- Media]), maps:get(media_aliases, Entry, #{})))
    },
    z_import_feed_native:prepare(Entry, Item, Dependents, FeedSettings, Context).

prepare_media_props(Uri, Entry, Common, Context) ->
    GuessedMime = z_media_identify:guess_mime(Uri),
    GuessedCategory = m_media:mime_to_category(GuessedMime),
    MediaCategory = case m_category:is_a(GuessedCategory, media, Context) of
                      true -> GuessedCategory;
                      false -> media
                  end,
    #{
        uri => z_import_feed_media:identity(Uri),
        medium => Uri,
        props => Common#{
                    <<"category_id">> => m_rsc:rid(MediaCategory, Context),
                    <<"title">> => z_html:unescape(maps:get(title, Entry)),
                    <<"is_dependent">> => true
                }
     }.

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
run(Entry, FeedSettings, Force, Context) ->
    FeedId = maps:get(id, FeedSettings),
    Restore = maps:get(refetch_deleted_once, FeedSettings, false),
    ImportContext = z_context:set(import_feed_refetch_deleted, Restore,
        z_context:set(import_feed_log_id, FeedId, Context)),
    Event = #{
        event => <<"entry">>,
        status => <<"started">>,
        uri => maps:get(uri, Entry, undefined),
        user_id => z_acl:user(Context),
        category_id => maps:get(category_id, FeedSettings, undefined),
        content_group_id => maps:get(content_group_id, FeedSettings, undefined)
    },
    ok = m_import_feed_log:write(FeedId, Event, Context),
    m_import_feed_log:changed(FeedId, Context),
    Result = try
        run_entry(Entry, FeedSettings, Force, ImportContext)
    catch Class:Reason ->
        {error, {Class, Reason}}
    end,
    {Status, Message} = case Result of
        {error, Error} -> {<<"error">>, m_import_feed:error_message(Error, Context)};
        Success -> {atom_to_binary(Success), undefined}
    end,
    ok = m_import_feed_log:write(FeedId, Event#{status := Status, message => Message}, Context),
    m_import_feed_log:changed(FeedId, Context),
    Result.

run_entry(#{uri := undefined}, _, _, _) ->
    {error, missing_identity};
run_entry(Entry, FeedSettings, Force, Context) ->
    {Item, Dependents} = prepare(Entry, FeedSettings, Context),
    Uri = maps:get(uri, Item),
    Hash = crypto:hash(sha256, term_to_binary({Item, Dependents})),
    FeedId = maps:get(id, FeedSettings),
    %% Acquire all known identities in a stable order, including shared media.
    %% Resource/media APIs manage their own short write transactions.
    Uris = lists:usort([Uri | [maps:get(uri, Dependent) || Dependent <- Dependents]]),
    try with_identities(Uris, fun() ->
        RestoreDeleted = z_context:get(import_feed_refetch_deleted, Context) =:= true
            andalso lists:any(fun(SourceUri) ->
                maps:is_key(SourceUri, maps:get(refetch_deleted_uris, FeedSettings, #{}))
                    orelse is_tombstone(SourceUri, Context)
            end, Uris),
        case z_db:q1("
                select checksum
                from import_feed_item
                where feed_id=$1
                  and uri=$2
                  and status='ok'
                ",
                [FeedId, Uri], Context) of
            Hash when not Force, not RestoreDeleted -> skipped;
            _ ->
                case deleted(Uri, Context) of
                    true -> record(FeedId, Uri, <<"article">>, <<"deleted">>, Hash, undefined, Context), skipped;
                    false ->
                        Notice = #import_resource{
                            item = Item,
                            related = Dependents,
                            source = #{
                                importer => mod_import_feed,
                                feed => maps:without([user_groups, lease_token, refetch_deleted_uris], FeedSettings)
                            },
                            options = maps:get(native_options, Item, []),
                            raw = maps:get(raw, Entry)
                        },
                        Existing = lookup(Uri, Context),
                        Default = fun(ImportContext) ->
                            {Id, Imported} = import(Item, Dependents, ImportContext),
                            {ok, Id, Imported}
                        end,
                        case z_import_resource:import(Notice, Default, Context) of
                            {ok, Id, Imported} ->
                                finish(Id, Imported, Existing, Uri, FeedId, Hash, Context);
                            {skip, Reason} ->
                                record(FeedId, Uri, <<"article">>, <<"skipped">>, Hash, Reason, Context), skipped;
                            {error, Reason} ->
                                throw({import_error, Reason})
                        end
                end
        end
    end, Context)
    catch
        throw:{import_error, Reason} ->
            record(FeedId, Uri, <<"article">>, <<"error">>, undefined, Reason, Context),
            {error, Reason}
    end.

%% Locks are shared by connected Erlang nodes and released if a worker exits.
%% Sorted acquisition prevents deadlocks for feeds sharing media in reverse order.
with_identities([], Import, _Context) ->
    Import();
with_identities([Uri | Rest], Import, Context) ->
    Lock = {{?MODULE, z_context:site(Context), Uri}, self()},
    global:trans(Lock, fun() -> with_identities(Rest, Import, Context) end).

finish(Id, Imported, Existing, Uri, FeedId, Hash, Context) ->
    true = is_integer(Id) andalso z_acl:rsc_editable(Id, Context),
    remember(Uri, Id, Context),
    maps:foreach(
        fun(MediaUri, ResourceId) ->
            true = is_integer(ResourceId) andalso z_acl:rsc_visible(ResourceId, Context),
            remember(MediaUri, ResourceId, Context),
            Kind = case m_rsc:is_a(ResourceId, media, Context) of
                        true -> <<"media">>;
                        false -> <<"resource">>
                   end,
            record(FeedId, MediaUri, Kind, <<"ok">>, undefined, undefined, Context)
        end,
        Imported),
    record(FeedId, Uri, <<"article">>, <<"ok">>, Hash, undefined, Context),
    case Existing of
        undefined -> imported;
        _ -> updated
    end.

import(#{zotonic_export := _} = Item, _, Context) ->
    {Id, Imported} = z_import_feed_native:run(Item, lookup(maps:get(uri, Item), Context), Context),
    lists:foreach(fun({Predicate, Object}) ->
                      edge(Id, Predicate, Object, Context)
                  end,
                  maps:get(default_connections, Item)),
    {Id, Imported};
import(Item, Dependents, Context) ->
    %% Reuse media by source URI. Do not steal media from existing local resources.
    MediaResults = lists:foldl(
        fun(Dependent, Acc) ->
               Uri = maps:get(uri, Dependent),
               case deleted(Uri, Context) of
                    true ->
                        Acc#{Uri => undefined};
                    false ->
                       Id = optional_medium(
                            Uri,
                            fun() ->
                                MediaId = upsert(Dependent, Context),
                                MediaId
                            end,
                            Context),
                       Acc#{Uri => Id}
               end
        end,
        #{},
        Dependents),
    Imported = maps:filter(fun(_, MediaId) -> is_integer(MediaId) end, MediaResults),
    Props = local_body(maps:get(props, Item), media_aliases(Item, MediaResults), Context),
    Item1 = Item#{
        props := Props
    },
    Id = upsert(Item1, Context),
    case maps:get(medium, Item) of
        undefined ->
            ok;
        Attached ->
            AttachedId = Id,
            LocalProps = local_body(
                maps:get(props, Item),
                media_aliases(Item, MediaResults#{z_import_feed_media:identity(Attached) => AttachedId}),
                Context),
            case m_rsc:update(Id, maps:with([<<"body">>], LocalProps), Context) of
                {ok, _} -> ok;
                {error, Reason} -> throw({import_error, Reason})
            end
    end,
    lists:foreach(
        fun
            ({Predicate, Object}) when is_integer(Object) ->
                edge(Id, Predicate, Object, Context);
            ({Predicate, MediaUri}) ->
                case maps:find(MediaUri, Imported) of
                    {ok, Object} ->
                        edge(Id, Predicate, Object, Context);
                    error ->
                        ok
                end
        end,
        maps:get(edges, Item)),
    %% All body embeds and connections exist before a download can start.
    lists:foreach(
        fun(Dependent) ->
            case maps:find(maps:get(uri, Dependent), Imported) of
                {ok, MediaId} -> queue_medium(MediaId, maps:get(medium, Dependent), Context);
                error -> ok
            end
        end,
        Dependents),
    queue_medium(Id, maps:get(medium, Item), Context),
    {Id, Imported}.

queue_medium(_, undefined, _) ->
    ok;
queue_medium(Id, Url, Context) ->
    case z_import_feed_media:queue(z_context:get(import_feed_log_id, Context), Id, Url, Context) of
        ok -> ok;
        {error, Reason} -> throw({import_error, Reason})
    end.

%% Media are optional. Their own APIs handle write transactions; a failed
%% download or insertion must not discard the rest of the article.
optional_medium(Uri, Import, Context) ->
    try Import()
    catch
        throw:{import_error, Reason} when
                Reason =:= eacces;
                element(1, Reason) =:= media;
                element(1, Reason) =:= media_fetch ->
            ?LOG_WARNING(#{
                in => zotonic_mod_import_feed,
                text => <<"Skipping unavailable feed medium">>,
                uri => Uri,
                result => error,
                reason => Reason
            }),
            ok = m_import_feed_log:write(
                    z_context:get(import_feed_log_id, Context),
                    #{
                        event => <<"media">>,
                        status => <<"error">>,
                        uri => Uri,
                        user_id => z_acl:user(Context),
                        message => m_import_feed:error_message(Reason, Context)
                    },
                    Context),
            undefined
    end.

media_aliases(Item, Imported) ->
    maps:fold(
        fun(Alias, Selected, Acc) ->
            case maps:find(Selected, Imported) of
                {ok, Id} -> Acc#{Alias => Id};
                error -> Acc
            end
        end,
        Imported,
        maps:get(media_aliases, Item, #{})).

%% @doc Replace imported inline images with editable Zotonic media embeds.
%% The map includes source variant aliases. An undefined ID removes references
%% to a medium that was skipped or could not be imported.
-spec local_body(Props, Imported, Context) -> map()
    when
        Props :: map(),
        Imported :: #{binary() => integer() | undefined},
        Context :: z:context().
local_body(#{<<"body">> := Body} = Props, Imported, Context) ->
    Urls = maps:fold(
        fun
            (_, undefined, Acc) ->
                Acc;
            (MediaUri, Id, Acc) ->
                case z_media_tag:url(Id, [], Context) of
                    {ok, Local} when Local =/= <<>> ->
                        Acc#{MediaUri => Local};
                    _ ->
                        Acc
                end
        end,
        #{},
        Imported),
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            Html = iolist_to_binary([
                body_html(local_node(Node, Imported, Urls, maps:get(<<"website">>, Props, <<>>)))
                || Node <- Children
            ]),
            Props#{
                <<"body">> := normalize_links(Html, maps:get(<<"website">>, Props, <<>>), Context)
            };
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
local_node({<<"a">>, Attributes, [{<<"img">>, _, _}]} = Node, Imported, Urls, Base) ->
    case maps:find(proplists:get_value(<<"href">>, Attributes), Imported) of
        {ok, Id} when is_integer(Id) -> media_embed(Id);
        {ok, undefined} -> <<>>;
        error -> local_element(Node, Imported, Urls, Base)
    end;
local_node({<<"img">>, _, _} = Node, Imported, Urls, Base) ->
    [Selected | _] = z_import_feed_parse:images(
        iolist_to_binary(z_html_parse:to_html(Node)), Base) ++ [undefined],
    case maps:find(Selected, Imported) of
        {ok, Id} when is_integer(Id) -> media_embed(Id);
        {ok, undefined} -> <<>>;
        error -> local_element(Node, Imported, Urls, Base)
    end;
local_node({_, _, _} = Node, Imported, Urls, Base) ->
    local_element(Node, Imported, Urls, Base);
local_node(Node, _, _, _) -> Node.

media_embed(Id) ->
    {comment, iolist_to_binary([" z-media ", integer_to_binary(Id),
        " {\"size\":\"large\",\"align\":\"block\"} "])}.

local_element({Tag, Attributes, Children}, Imported, Urls, Base) ->
    IsFailed = fun(<<>>) -> false;
                  (Value) ->
        maps:find(z_import_feed_http:resolve(Value, Base), Imported) =:= {ok, undefined}
    end,
    case IsFailed(proplists:get_value(<<"src">>, Attributes, <<>>)) of
        true -> <<>>;
        false ->
            Attrs1 = [
                {Attribute, case lists:member(Attribute, [<<"src">>, <<"href">>, <<"poster">>]) of
                                true -> maps:get(Value, Urls, Value);
                                false -> Value end
                }
                || {Attribute, Value} <- Attributes,
                   not (lists:member(Attribute, [<<"href">>, <<"poster">>]) andalso IsFailed(Value))
            ],
            {Tag, Attrs1, [local_node(Node, Imported, Urls, Base) || Node <- Children]}
    end.

%% @doc Remove image hyperlinks and resolve article links against the source.
%% Only resources recorded as imported articles can become local destinations.
-spec normalize_links(Body, Base, Context) -> binary() when
    Body :: binary(), Base :: binary(), Context :: z:context().
normalize_links(Body, Base, Context) ->
    case z_html_parse:parse(<<"<div>", Body/binary, "</div>">>, #{mode => html, escape => false}) of
        {ok, {_, _, Children}} ->
            iolist_to_binary([body_html(Node) || Node <- link_nodes(Children, Base, Context)]);
        _ ->
            Body
    end.

link_nodes(Nodes, Base, Context) ->
    lists:flatmap(
        fun
            ({<<"a">>, Attributes, Children}) ->
                Href = proplists:get_value(<<"href">>, Attributes, <<>>),
                Resolved = z_import_feed_http:resolve(Href, Base),
                Content = link_nodes(Children, Base, Context),
                case image_link(z_import_feed_media:resolve(Href, Base)) of
                    true -> Content;
                    false ->
                        Link = case Resolved of
                            <<>> -> Href;
                            _ -> article_link(Resolved, Context)
                        end,
                        [{<<"a">>, lists:keystore(<<"href">>, 1, Attributes, {<<"href">>, Link}), Content}]
                end;
            ({Tag, Attributes, Children}) ->
                [{Tag, Attributes, link_nodes(Children, Base, Context)}];
            (Node) ->
                [Node]
        end,
        Nodes).

image_link(<<"data:image/", _/binary>>) -> true;
image_link(<<>>) -> false;
image_link(Url) ->
    Path = maps:get(path, uri_string:parse(Url), <<>>),
    re:run(
        Path,
        <<"\\.(?:jpe?g|png|gif|webp|avif|svg|tiff?|bmp|ico)$">>,
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
                        FragmentValue -> <<"#", FragmentValue/binary>>
                    end,
                    iolist_to_binary([Page, Fragment])
            end;
        false ->
            Url
    end.

imported_article(Source, Context) ->
    case z_db:q1("
        select f.rsc_id
        from import_feed_resource f
            join import_feed_item i on i.uri=f.uri
        where f.uri=$1
          and i.kind='article'
          and f.rsc_id is not null
        limit 1", [Source], Context)
    of
        undefined ->
            %% Resource properties can be stored as either term or JSON data.
            %% Read website via the model rather than depending on that storage.
            Ids = z_db:q("
                select distinct f.rsc_id
                from import_feed_resource f
                    join import_feed_item i on i.uri=f.uri
                where i.kind='article'
                  and f.rsc_id is not null
                ", Context),
            find_article_website(Source, Ids, Context);
        Id ->
            Id
    end.

find_article_website(_, [], _) -> undefined;
find_article_website(Source, [{Id} | Rest], Context) ->
    case z_html:unescape(m_rsc:p(Id, <<"website">>, Context)) of
        Source -> Id;
        _ -> find_article_website(Source, Rest, Context)
    end.

edge(Id, Predicate, Object, Context) ->
    case m_edge:insert(Id, Predicate, Object, Context) of
        {ok, _} -> ok;
        {error, Reason} -> throw({import_error, {edge, Reason}})
    end.

upsert(#{uri := Uri, props := ItemProps} = Item, Context) ->
    Authority = maps:get(<<"is_authoritative">>, ItemProps, false),
    Props = ItemProps#{<<"uri">> => case Authority of
                                true -> undefined;
                                false -> Uri
                            end,
               <<"feed_source_uri">> => Uri},
    Kind = case maps:get(<<"is_dependent">>, ItemProps, false) of
        true -> <<"media">>;
        false -> <<"article">>
    end,
    FeedId = z_context:get(import_feed_log_id, Context),
    WriteContext = z_context:set(import_feed_identity, {FeedId, Uri, Kind}, Context),
    Result = case lookup(Uri, Context) of
                 undefined ->
                     insert_item(Item, Props, WriteContext);
                 Id ->
                     %% Only a resource already mapped by this importer may be authoritative.
                     Owned = z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context) =:= Id,
                     case Owned orelse not m_rsc:p(Id, <<"is_authoritative">>, Context) of
                         true ->
                             UpdateProps = case maps:get(<<"is_dependent">>, ItemProps, false) of
                                               true ->
                                                   case m_rsc:is_a(Id, media, Context) of
                                                       true ->
                                                           maps:remove(<<"category_id">>, Props);
                                                       false ->
                                                           throw({import_error, media_identity_conflict})
                                                   end;
                                               false ->
                                                   Props
                                           end,
                             m_rsc:update(Id, UpdateProps, [is_import, {tz, <<"UTC">>}], WriteContext);
                         false ->
                             {error, authoritative_resource}
                     end
             end,
    case Result of
        {ok, ImportedId} ->
            ImportedId;
        {error, Reason} ->
            throw({import_error, Reason})
    end.

%% A resource is enough for body embeds and edges while its medium is pending.
%% The task's m_media:replace_file call identifies the file and sets its category.
insert_item(_, Props, Context) ->
    m_rsc:insert(Props, [is_import, {tz, <<"UTC">>}], Context).

%% @doc Called by rsc_update inside the resource write transaction. Insertion
%% already allocated the resource ID, so the foreign key is valid. A later
%% observer or database failure rolls back both resource and administration.
%% Never mark the entry complete here: its edges and media tasks are not ready.
-spec track_resource(FeedId, Uri, Kind, Id, Context) -> ok | {error, term()} when
    FeedId :: integer(), Uri :: binary(), Kind :: binary(), Id :: integer(), Context :: z:context().
track_resource(FeedId, Uri, Kind, Id, Context) ->
    case z_db:q1("
            insert into import_feed_resource(uri,rsc_id)
                values($1,$2)
            on conflict(uri) do
                update set
                    rsc_id=excluded.rsc_id
                where import_feed_resource.rsc_id=excluded.rsc_id
                   or (import_feed_resource.rsc_id is null and $3)
            returning rsc_id", [Uri, Id, z_context:get(import_feed_refetch_deleted, Context) =:= true], Context) of
        Id ->
            z_db:q("
                insert into import_feed_item(feed_id,uri,kind,status)
                    values($1,$2,$3,'importing')
                on conflict(feed_id,uri) do
                    update set
                        kind=excluded.kind,
                        status='importing',
                        checksum=null,
                        last_error=null,
                        modified=now()
                ", [FeedId, Uri, Kind], Context),
            ok;
        undefined ->
            {error, import_identity_conflict}
    end.

lookup(Uri, Context) ->
    case z_db:q1("select rsc_id from import_feed_resource where uri=$1", [Uri], Context) of
        undefined -> m_rsc:rid(Uri, Context);
        Id -> Id
    end.
deleted(Uri, Context) ->
    z_context:get(import_feed_refetch_deleted, Context) =/= true
    andalso is_tombstone(Uri, Context).
is_tombstone(Uri, Context) ->
    z_db:q1("select count(*) from import_feed_resource where uri=$1 and rsc_id is null", [Uri], Context) =:= 1.
remember(Uri, Id, Context) ->
    z_db:q("insert into import_feed_resource(uri,rsc_id) values($1,$2)
        on conflict(uri) do update set rsc_id=excluded.rsc_id", [Uri, Id], Context), ok.
record(FeedId, Uri, Kind, Status, Hash, Error, Context) ->
    Message = case Error of undefined -> undefined; _ -> iolist_to_binary(io_lib:format("~0P", [Error, 8])) end,
    z_db:q("
        insert into import_feed_item(feed_id,uri,kind,status,checksum,last_error)
            values($1,$2,$3,$4,$5,$6)
        on conflict(feed_id,uri) do
            update set
                status=excluded.status,
                checksum=excluded.checksum,
                last_error=excluded.last_error,
                modified=now()
        ",
        [FeedId, Uri, Kind, Status, Hash, Message], Context),
    Id = lookup(Uri, Context),
    m_import_feed_log:write(FeedId, #{
            event => Kind,
            status => Status,
            uri => Uri,
            user_id => z_acl:user(Context),
            rsc_id => Id,
            category_id => m_rsc:p(Id, category_id, Context),
            content_group_id => m_rsc:p(Id, content_group_id, Context),
            message => Message
        }, Context).

category(Entry, #{guess_category := true, category_id := Default}, Context) ->
    Tags = [z_string:to_lower(Tag) || Tag <- maps:get(tags, Entry)],
    Candidates = [Id || Category <- m_category:tree_flat(Context),
        Id <- [proplists:get_value(id, Category)], is_integer(Id),
        not m_category:is_a(Id, meta, Context),
        lists:any(fun(Tag) -> lists:member(Tag, Tags) end,
            [z_string:to_lower(z_convert:to_binary(m_rsc:p(Id, <<"name">>, Context))),
             z_string:to_lower(z_trans:lookup_fallback(m_rsc:p(Id, <<"title">>, Context), Context))])],
    case Candidates of
        [Id | _] ->
            Id;
        [] ->
            %% The configured category might have moved under meta since saving.
            case m_category:is_a(Default, meta, Context) of
                true -> m_rsc:rid(article, Context);
                false -> Default
            end
    end;
category(_, FeedSettings, _) ->
    maps:get(category_id, FeedSettings).

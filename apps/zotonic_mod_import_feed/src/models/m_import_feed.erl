%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Feed discovery, previews, settings, statistics and import history.
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

-module(m_import_feed).
-define(DEFAULT_POLL_MINUTES, 60).
-define(MIN_POLL_MINUTES, 5).
-define(MAX_POLL_MINUTES, 10080).
-define(DEFAULT_IMPORT_DEPTH, 1).
-define(MAX_IMPORT_DEPTH, 10).
-define(PREVIEW_ENTRIES, 3).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model", "import_and_migration", "import", "structured_data", "url"
    ]
}).
-moduledoc("
Discover, preview and manage the feeds imported by `mod_import_feed`.

Administrator accounts cannot run imports. Select an enabled non-administrator account
with only the required import permissions. This is checked when saving settings and
before polling, including for existing feeds whose account is later promoted to admin.

## Model paths

The model exposes read-only GET paths. All paths require `use` permission for
`mod_import_feed`; denied requests return `{error, eacces}`. Unknown paths return
`{error, unknown_path}`. Successful `m_get/3` calls return `{ok, {Value, Rest}}`.

| Template expression | GET path | Value |
| --- | --- | --- |
| `m.import_feed.can_select_user` | `can_select_user` | Whether the caller may choose an import account. |
| `m.import_feed.list` | `list` | Feed maps, newest first, including statistics and `article_count`. |
| `m.import_feed.get[id]` | `get/<id>` | One feed map; `enoent` when missing. |
| `m.import_feed.items[id]` | `items/<id>` | First page of import history, up to 100 rows. |
| `m.import_feed.history[id][offset]` | `history/<id>/<offset>` | Map with `items`, `offset`, `previous`, `next` and `has_next`. |

Feed maps use atom keys and include settings, counters, poll times and `is_running`.
`is_zotonic_feed` records detected native resource entries: true for native or mixed
feeds, false for ordinary feeds, and undefined until known. Empty Atom feeds do
not overwrite an earlier detection.
`use_credentials` defaults to false. When true, fetches use host-matched OAuth2
consumer tokens registered for the selected import account. Tokens are not stored
in feed settings. Development fetch behavior follows `m_site:environment/1`.
Native import settings are `import_depth` (0–10, default 1), `is_subscribe` and
`is_subscribe_connections` (both default false). The subscription flags apply
through `mod_websub` to non-authoritative Zotonic resource imports.
Internal lease tokens and saved user groups are omitted. History rows include `uri`,
`rsc_id`, `kind`, `status` and processing times. Rows referring to resources the caller
cannot view are filtered out; deleted resources have an undefined `rsc_id`.

```django
{% for feed in m.import_feed.list %}
    <p>{{ feed.title|escape }}: {{ feed.article_count }} articles</p>
{% endfor %}
```

## Erlang API

Discovery, previews and writes are direct Erlang functions, not model POST paths.
The admin screens invoke them through `mod_import_feed` events.

`fetch_resource(Uri, Context)` implements the optional `rsc_import_fetch` handler.
It returns `undefined` for native Zotonic resources (which use the core importer)
and unknown URIs, `{ok, Id}` on refresh, or an error. Known
articles require module write permission and resource edit permission from the
caller, plus an enabled non-admin import account with permission to edit the article.
It refreshes one entry from the current feed, including for paused feeds, using
saved settings and WordPress enrichment. Missing entries return `feed_entry_not_found`.
The explicit refresh bypasses checksums but preserves deletion tombstones.

- `discover(Url, Context)` returns `{ok, Feeds}`, where each candidate has `url` and
  `title`, or `{error, Reason}`. It accepts a website URL or a feed URL.
- `preview(Url, Context)` returns `{ok, Feed}` with at most three example `entries`,
  using WordPress enrichment. No resources are imported.
- `preview(Settings, Count, Context)` uses atom-keyed settings, such as a map from
  `get/2`, and limits the result to at most three entries. With category settings,
  it also reports `target_category`, `dependent_count` and `attached_medium` per entry.
- `get(Id, Context)` returns `{ok, Feed}` or `{error, enoent}`.
- `list(Context)` returns feed maps. `items(Id, Offset, Context)` returns up to
  100 history rows, with a non-negative integer offset. Both return `[]` on ACL denial.
- `save(undefined, Input, Context)` creates a feed; `save(Id, Input, Context)` replaces
  its settings. Both return `{ok, Id}` or `{error, Reason}`. The feed URL is immutable.
- `pause(Id, Boolean, Context)` pauses or resumes a feed and returns `ok` or an error.
- `queue(Id, Reimport, Context)` marks a feed due and starts background processing.
  With `Reimport = true`, it clears entry checksums, preserving source identities and
  deletion records. It returns `ok` or an error, does not wait for imports to finish,
  and does not resume a paused feed.

`can_select_user/1` checks both feed access and the account-selection permission.
`validate_user/2` additionally rejects read-only contexts, missing accounts, resources
without user credentials and disabled accounts; it returns `{ok, UserId}`,
`{error, eacces}` or `{error, invalid_import_user}`.

These administration functions check module permission; writes also reject read-only
contexts. `allowed/1` reports module permission only. Exported `install/1` and `decode/1`
are internal schema/worker helpers and do not form an ACL-protected administration API.

## Saving settings

`save/3` accepts a complete map with binary keys, matching the admin form. Keys are
`url`, `title`, `category_id`, `content_group_id`, `guess_category`, `connections`,
`media_mode`, `poll_minutes`, `is_authoritative`, `use_wordpress`, `is_paused` and
`user_id`. The optional `user_id` selects an enabled account; selecting another account
requires administrator status or `use` permission for `mod_admin_identity`. Omission
on creation uses the current user; omission on update preserves the saved identity
and groups. Submitted group memberships are never accepted.
Category and content-group values can be resource IDs or names. Connections are a
binary with one `predicate=resource` per line, or a list of such binaries from the
connection picker. At most 50 connections are accepted and duplicates are removed. Media mode is `dependent`, `attached`
or `none`, as a binary. Polling accepts 5 to 10080 minutes.

Omitted booleans are false in this API, including `use_wordpress`; the admin form
explicitly enables WordPress enrichment. The defaults for category, media mode and
polling are Article, dependent media and 60 minutes. Changing the selected user records that account's groups for future imports. Ordinary
settings saves preserve them. Saving makes the feed due immediately unless paused.

```erlang
{ok, FeedId} = m_import_feed:save(undefined, #{
    <<\"url\">> => <<\"https://example.org/feed/\">>,
    <<\"title\">> => <<\"Example articles\">>,
    <<\"category_id\">> => <<\"article\">>,
    <<\"use_wordpress\">> => true,
    <<\"is_paused\">> => true
}, Context).
```

Common errors include `eacces`, `invalid_settings`, `feed_url_is_immutable`,
`save_failed_or_duplicate_url` and `enoent`. Discovery and previews also propagate
fetch and parse errors. See `mod_import_feed` for import semantics and notifications.
").
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(zotonic_model).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([record_feed_type/3, m_get/3, allowed/1, discover/2, preview/2, preview/3, save/3, pause/3, queue/3,
    get/2, list/1, items/3, can_select_user/1, validate_user/2, validate_connection/3, is_admin_user/2, error_message/2, delete_content/2, delete_feed/3, install/1, decode/1, fetch_resource/2]).

%% @doc Optional rsc_import_fetch handler. Known articles require module write
%% permission and edit permission on the article before using the saved account.
-spec fetch_resource(Uri, Context) -> undefined | {ok, integer()} | {error, term()}
    when Uri :: binary(), Context :: z:context().
fetch_resource(Uri, C) ->
    case z_db:q_row("select i.feed_id, r.rsc_id from import_feed_item i
            join import_feed_resource r on r.uri=i.uri
            where i.uri=$1 and i.kind='article' order by i.modified desc, i.feed_id limit 1", [Uri], C) of
        undefined -> undefined;
        {_, undefined} -> {error, enoent};
        {FeedId, Id} ->
            case m_rsc:p_no_acl(Id, feed_is_zotonic, C) of
                true -> undefined;
                _ ->
                    case writable(C) andalso z_acl:rsc_editable(Id, C) of
                        true -> z_import_feed_fetch:run(FeedId, Id, Uri, C);
                        false -> {error, eacces}
                    end
            end
    end.

-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get(Path, _Msg, Context) ->
    case allowed(Context) of
        true -> model(Path, Context);
        false -> {error, eacces}
    end.
model([<<"user_is_admin">>, User | Rest], C) ->
    {ok, {is_admin_user(m_rsc:rid(User, C), C), Rest}};
model([<<"can_select_user">> | Rest], C) -> {ok, {can_select_user(C), Rest}};
model([<<"list">> | Rest], C) -> {ok, {list(C), Rest}};
model([<<"get">>, Id | Rest], C) ->
    case get(Id, C) of {ok, F} -> {ok, {F, Rest}}; E -> E end;
model([<<"history">>, Id, Offset | Rest], C) ->
    N = case integer(Offset) of undefined -> 0; I -> I end,
    Rows = items(Id, N, C),
    {ok, {#{items => Rows, offset => N, previous => max(0, N - 100),
        next => N + 100, has_next => length(Rows) =:= 100}, Rest}};
model([<<"items">>, Id | Rest], C) -> {ok, {items(Id, 0, C), Rest}};
model(_, _) -> {error, unknown_path}.

-spec allowed(z:context()) -> boolean().
allowed(C) ->
    z_acl:is_allowed(use, mod_import_feed, C).

writable(C) ->
    allowed(C) andalso not z_acl:is_read_only(C).

-spec list(z:context()) -> [map()].
list(C) ->
    case allowed(C) of
        true ->
            {ok, Rows} = z_db:qmap("select f.*,
                (select count(*) from import_feed_item i join import_feed_resource r on r.uri=i.uri
                    where i.feed_id=f.id and i.kind='article' and r.rsc_id is not null) as article_count
                from import_feed f order by f.id desc", [], [{keys, atom}], C),
            [public(decode(R)) || R <- Rows];
        false -> []
    end.

-spec get(term(), z:context()) -> {ok, map()} | {error, term()}.
get(Id, C) ->
    case allowed(C) of
        false -> {error, eacces};
        true ->
            case z_db:qmap("select * from import_feed where id=$1", [integer(Id)], [{keys, atom}], C) of
                {ok, [R]} -> {ok, public(decode(R))};
                {ok, []} -> {error, enoent};
                E -> E
            end
    end.

-spec items(term(), non_neg_integer(), z:context()) -> [map()].
items(Id, Offset, C) when is_integer(Offset), Offset >= 0 ->
    case allowed(C) of
        false -> [];
        true ->
            {ok, Rows} = z_db:qmap("select i.*, r.rsc_id from import_feed_item i
                left join import_feed_resource r on r.uri=i.uri
                where i.feed_id=$1 order by i.modified desc, i.uri limit 100 offset $2",
                [integer(Id), Offset], [{keys, atom}], C),
            [R || R <- Rows, maps:get(rsc_id, R) =:= undefined orelse
                z_acl:rsc_visible(maps:get(rsc_id, R), C)]
    end.

%% Persist observed format without changing settings or interrupting an import lease.
%% Internal helper: callers have already authorized preview, save or polling.
-spec record_feed_type(map(), map(), z:context()) -> ok.
record_feed_type(#{id := Id, url := Url}, #{is_zotonic_feed := IsZotonic}, C)
    when is_integer(Id), is_boolean(IsZotonic) ->
    z_db:q("update import_feed set is_zotonic_feed=$3 where id=$1 and url=$2",
        [Id, Url, IsZotonic], C),
    ok;
record_feed_type(_, _, _) -> ok.

-spec discover(binary(), z:context()) -> {ok, [map()]} | {error, term()}.
discover(Url, C) ->
    case allowed(C) of
        false -> {error, eacces};
        true ->
            case z_import_feed_http:get(Url, C) of
                {ok, {Final, Headers, _, Data}} -> z_import_feed_parse:discover(Data, z_convert:to_binary(Final), Headers);
                E -> E
            end
    end.

-spec preview(binary(), z:context()) -> {ok, map()} | {error, term()}.
preview(Url, C) -> preview(#{url => Url, use_wordpress => true}, ?PREVIEW_ENTRIES, C).

-spec preview(map(), pos_integer(), z:context()) -> {ok, map()} | {error, term()}.
preview(Settings, Count, C0) ->
    C = z_import_feed_http:context(Settings, C0),
    Url = maps:get(url, Settings),
    case allowed(C) of
        false -> {error, eacces};
        true ->
            case z_import_feed_http:get(Url, C) of
                {ok, {Final, _, _, Data}} ->
                    case z_import_feed_parse:parse(Data, z_convert:to_binary(Final)) of
                        {ok, F} -> {ok, F#{entries => [preview_entry(E, Settings, C)
                            || E <- lists:sublist(maps:get(entries, F), min(Count, ?PREVIEW_ENTRIES))]}};
                        E -> E
                    end;
                E -> E
            end
    end.

preview_entry(E, Settings, C) ->
    Entry = case maps:get(use_wordpress, Settings, true) of
        true -> z_import_feed_wordpress:enrich(E, C);
        false -> E
    end,
    case maps:is_key(category_id, Settings) of
        true ->
            try z_import_feed_import:prepare(Entry, Settings, C) of
                {Item, Dependents} ->
                    TargetCategory = case maps:is_key(zotonic_export, Item) of
                        true -> undefined;
                        false -> maps:get(<<"category_id">>, maps:get(props, Item))
                    end,
                    Entry#{target_category => TargetCategory,
                        dependent_count => length(Dependents), attached_medium => maps:get(medium, Item)}
            catch throw:{import_error, Reason} -> Entry#{preview_error => error_message(Reason, C)} end;
        false -> Entry
    end.

-spec save(term(), map(), z:context()) -> {ok, integer()} | {error, term()}.
save(Id, Input, C) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            case save_settings(integer(Id), Input, C) of
                {ok, Props} ->
                    P = Props#{next_poll => calendar:universal_time(), last_error => undefined},
                    case integer(Id) of
                        undefined ->
                            case z_db:insert(import_feed, P, C) of
                                {ok, _} = Ok -> Ok;
                                {error, _} -> {error, save_failed_or_duplicate_url}
                            end;
                        FeedId ->
                            case get(FeedId, C) of
                                {ok, Existing} ->
                                    %% A changed URL is a different feed, not an edit of history.
                                    case maps:get(url, Existing) =:= maps:get(url, P) of
                                        true ->
                                            case z_db:update(import_feed, FeedId, P#{modified => calendar:universal_time()}, C) of
                                                {ok, 1} -> {ok, FeedId};
                                                _ -> {error, save_failed}
                                            end;
                                        false -> {error, feed_url_is_immutable}
                                    end;
                                E -> E
                            end
                    end;
                E -> E
            end
    end.

%% Choosing an execution identity is distinct from ordinary feed editing.
-spec can_select_user(Context) -> boolean() when Context :: z:context().
can_select_user(C) ->
    allowed(C) andalso (z_acl:is_admin(C) orelse z_acl:is_allowed(use, mod_admin_identity, C)).

-spec validate_user(User, Context) -> {ok, integer()} | {error, term()}
    when User :: term(), Context :: z:context().
validate_user(User, C) ->
    case can_select_user(C) andalso not z_acl:is_read_only(C) of
        false -> {error, eacces};
        true ->
            Id = m_rsc:rid(User, C),
            case is_integer(Id) andalso m_rsc:exists(Id, C)
                andalso (Id =:= 1 orelse m_identity:is_user(Id, C))
                andalso z_auth:is_enabled(Id, C)
            of
                true ->
                    case is_admin_user(Id, C) of
                        true -> {error, admin_import_user};
                        false -> {ok, Id}
                    end;
                false -> {error, invalid_import_user}
            end
    end.

save_settings(Id, Input, C) ->
    case import_user(Id, Input, C) of
        {ok, UserProps} ->
            case validate_settings(Input, C) of
                {ok, Props} -> {ok, maps:merge(Props, UserProps)};
                Error -> Error
            end;
        Error -> Error
    end.

import_user(undefined, Input, C) ->
    Current = z_acl:user(C),
    Requested = maps:get(<<"user_id">>, Input, Current),
    case integer(Requested) =:= Current andalso is_integer(Current) of
        true ->
            case is_admin_user(Current, C) of
                true -> {error, admin_import_user};
                false -> {ok, user_props(Current, C)}
            end;
        false -> select_user(Requested, C)
    end;
import_user(Id, Input, C) ->
    case get(Id, C) of
        {ok, #{user_id := Current}} ->
            case maps:find(<<"user_id">>, Input) of
                error -> preserve_user(Current, C);
                {ok, Requested} ->
                    case integer(Requested) =:= Current of
                        true -> preserve_user(Current, C);
                        false -> select_user(Requested, C)
                    end
            end;
        Error -> Error
    end.

preserve_user(User, C) ->
    case is_admin_user(User, C) of
        true -> {error, admin_import_user};
        false -> {ok, #{}}
    end.

%% Check the account's full current privileges, not the worker's saved group limit.
-spec is_admin_user(User, Context) -> boolean()
    when User :: integer() | undefined, Context :: z:context().
is_admin_user(1, _) -> true;
is_admin_user(undefined, _) -> false;
is_admin_user(User, C) ->
    z_acl:is_admin(z_acl:logon(User, z_context:new(C))).

-spec error_message(Reason, Context) -> binary()
    when Reason :: term(), Context :: z:context().
error_message({invalid_regexp, Line, Reason}, C) ->
    iolist_to_binary([?__("Invalid removal regexp on line", C), " ", integer_to_binary(Line), ": ", Reason]);
error_message(feed_is_running, C) ->
    ?__("This feed is still importing. Pause it and wait for the current import to finish before deleting its content.", C);
error_message(admin_import_user, C) ->
    ?__("Administrator accounts cannot run feed imports. Importing external content with full administrator permissions is dangerous. Select a non-administrator account with only the permissions needed for importing.", C);
error_message(Reason, _) ->
    iolist_to_binary(io_lib:format("~0P", [Reason, 8])).

select_user(Requested, C) ->
    case validate_user(Requested, C) of
        {ok, User} ->
            %% Derive groups from the selected account, never from browser input.
            UserContext = z_acl:logon(User, z_context:new(C)),
            {ok, user_props(User, UserContext)};
        Error -> Error
    end.

user_props(User, C) ->
    #{user_id => User, user_groups => term_to_binary(z_acl:user_groups(C))}.

validate_settings(P, C) ->
    try settings(P, C) catch
        throw:{removal_patterns, Reason} -> {error, Reason};
        _:_ -> {error, invalid_settings}
    end.

settings(P, C) ->
    Patterns = z_convert:to_binary(maps:get(<<"remove_patterns">>, P, <<>>)),
    case z_import_feed_cleanup:compile(Patterns) of
        {ok, _} -> ok;
        {error, PatternError} -> throw({removal_patterns, PatternError})
    end,
    Url = maps:get(<<"url">>, P, <<>>),
    Cat = m_rsc:rid(maps:get(<<"category_id">>, P, <<"article">>), C),
    CG = m_rsc:rid(maps:get(<<"content_group_id">>, P, undefined), C),
    Minutes = integer(maps:get(<<"poll_minutes">>, P, ?DEFAULT_POLL_MINUTES)),
    Depth = integer(maps:get(<<"import_depth">>, P, ?DEFAULT_IMPORT_DEPTH)),
    Mode = maps:get(<<"media_mode">>, P, <<"dependent">>),
    Connections = parse_connections(maps:get(<<"connections">>, P, <<>>), C),
    case z_import_feed_http:url(Url)
        andalso is_integer(Minutes) andalso Minutes >= ?MIN_POLL_MINUTES andalso Minutes =< ?MAX_POLL_MINUTES
        andalso is_integer(Depth) andalso Depth >= 0 andalso Depth =< ?MAX_IMPORT_DEPTH
        andalso m_rsc:is_a(Cat, category, C) andalso not m_category:is_a(Cat, meta, C)
        andalso (CG =:= undefined orelse (z_module_manager:active(mod_content_groups, C)
            andalso m_rsc:is_a(CG, content_group, C)))
        andalso lists:member(Mode, [<<"dependent">>, <<"attached">>, <<"none">>])
        andalso is_list(Connections)
    of
        false -> {error, invalid_settings};
        true ->
            Props = #{<<"category_id">> => Cat, <<"content_group_id">> => CG},
            case z_acl:is_allowed(insert, #acl_rsc{category = m_category:id_to_name(Cat, C), props = Props}, C) of
                false -> {error, eacces};
                true -> {ok, maps:merge(copyright_settings(P, C), #{url => Url, title => z_convert:to_binary(maps:get(<<"title">>, P, Url)),
                    category_id => Cat, content_group_id => CG, remove_patterns => Patterns,
                    guess_category => bool(<<"guess_category">>, P),
                    connections => term_to_binary(Connections), media_mode => Mode,
                    poll_minutes => Minutes, is_authoritative => bool(<<"is_authoritative">>, P),
                    import_depth => Depth, use_credentials => bool(<<"use_credentials">>, P),
                    is_subscribe => bool(<<"is_subscribe">>, P),
                    is_subscribe_connections => bool(<<"is_subscribe_connections">>, P),
                    use_wordpress => bool(<<"use_wordpress">>, P), is_paused => bool(<<"is_paused">>, P)})}
            end
    end.

%% Preserve stored defaults while the optional copyright module is disabled.
copyright_settings(P, C) ->
    case z_module_manager:active(mod_copyright, C) of
        false -> #{};
        true ->
            Rights = copyright_text(<<"rights">>, P, 100),
            Licenses = m_copyright:list_other() ++ m_copyright:list_creative_commons()
                ++ m_copyright:list_rights_statements(),
            true = Rights =:= <<>> orelse lists:any(
                fun(#{<<"name">> := Name}) -> Name =:= Rights end, Licenses),
            #{rights => Rights,
                rights_attribution => copyright_text(<<"rights_attribution">>, P, 1000),
                rights_year => copyright_text(<<"rights_year">>, P, 100)}
    end.

copyright_text(Key, P, Max) ->
    Value = z_string:trim(z_convert:to_binary(maps:get(Key, P, <<>>))),
    true = byte_size(Value) =< Max,
    Value.

%% The picker only stages configuration; no resource edge is created here.
-spec validate_connection(Predicate, Object, Context) -> {ok, {integer(), integer()}} | {error, term()}
    when Predicate :: term(), Object :: term(), Context :: z:context().
validate_connection(Predicate, Object, C) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            P = m_rsc:rid(Predicate, C),
            O = m_rsc:rid(Object, C),
            case m_rsc:is_a(P, predicate, C) andalso is_integer(O)
                andalso z_acl:rsc_visible(P, C) andalso z_acl:rsc_visible(O, C)
            of
                true -> {ok, {P, O}};
                false -> {error, invalid_connections}
            end
    end.

%% One predicate=resource per line; IDs, names and resource URIs are accepted.
parse_connections(Values, C) when is_list(Values), length(Values) =< 50 ->
    case lists:all(fun is_binary/1, Values) of
        true -> parse_connections(iolist_to_binary(lists:join(<<"\n">>, Values)), C);
        false -> {error, invalid_connections}
    end;
parse_connections(B, C) when is_binary(B), byte_size(B) =< 16384 ->
    try
        Lines = [string:trim(L) || L <- binary:split(B, <<"\n">>, [global]), string:trim(L) =/= <<>>],
        true = length(Lines) =< 50,
        lists:usort([begin
            [Pred, Object] = binary:split(L, <<"=">>),
            P = m_rsc:rid(string:trim(Pred), C), O = m_rsc:rid(string:trim(Object), C),
            true = m_rsc:is_a(P, predicate, C), true = is_integer(O),
            true = z_acl:rsc_visible(O, C),
            {P, O}
        end || L <- Lines])
    catch _:_ -> {error, invalid_connections} end;
parse_connections(_, _) -> {error, invalid_connections}.

-spec pause(term(), boolean(), z:context()) -> ok | {error, term()}.
pause(Id, IsPaused, C) when is_boolean(IsPaused) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            case IsPaused of
                true -> pause_update(Id, true, C);
                false ->
                    case runnable_feed(Id, C) of
                        {ok, _} -> pause_update(Id, false, C);
                        Error -> Error
                    end
            end
    end.

pause_update(Id, IsPaused, C) ->
    case z_db:update(import_feed, integer(Id), #{is_paused => IsPaused,
            next_poll => calendar:universal_time(), modified => calendar:universal_time()}, C) of
        {ok, 1} -> ok;
        _ -> {error, enoent}
    end.

runnable_feed(Id, C) ->
    case get(Id, C) of
        {ok, #{user_id := User}} = Result ->
            case is_admin_user(User, C) of
                true -> {error, admin_import_user};
                false -> Result
            end;
        Error -> Error
    end.

%% Manual reimport clears successful hashes, not identities or deletion tombstones.
-spec queue(term(), boolean(), z:context()) -> ok | {error, term()}.
queue(Id, Reimport, C) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            case runnable_feed(Id, C) of
                {ok, _} ->
                    case Reimport of
                        true -> z_db:q("update import_feed_item set checksum=null where feed_id=$1", [integer(Id)], C);
                        false -> ok
                    end,
                    {ok, 1} = z_db:update(import_feed, integer(Id), #{next_poll => calendar:universal_time(), modified => calendar:universal_time()}, C),
                    mod_import_feed:observe_tick_1m(tick_1m, C),
                    ok;
                E -> E
            end
    end.

%% @doc Delete feed resources as the current user, retaining identity tombstones.
%% Pause atomically with the running-worker check so no new poll can claim it.
-spec delete_content(Id, Context) -> {ok, map()} | {error, term()} when
    Id :: term(), Context :: z:context().
delete_content(Id, C) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            case z_db:transaction(fun(Tx) -> delete_content_locked(integer(Id), Tx) end, C) of
                {rollback, Reason} -> {error, Reason};
                Result -> Result
            end
    end.

delete_content_locked(Id, C) ->
    case z_db:q_row("select lease_token from import_feed where id=$1 for update", [Id], C) of
        undefined -> {error, enoent};
        {undefined} ->
            {ok, 1} = z_db:update(import_feed, Id,
                #{is_paused => true, modified => calendar:universal_time()}, C),
            z_db:q("select pg_advisory_xact_lock(hashtext(current_schema()), 194815)", C),
            Rows = z_db:q("select distinct r.rsc_id from import_feed_resource r
                join import_feed_item i on i.uri=r.uri
                where i.feed_id=$1 and r.rsc_id is not null order by r.rsc_id", [Id], C),
            Counts = lists:foldl(fun({RscId}, Acc) ->
                Result = case deletable_tree(RscId, #{}, C) of
                    false -> skipped;
                    true -> case m_rsc:delete(RscId, C) of
                        ok -> deleted;
                        {error, enoent} -> skipped;
                        {error, eacces} -> skipped;
                        {error, _} -> failed
                    end
                end,
                Acc#{Result := maps:get(Result, Acc) + 1}
            end, #{deleted => 0, skipped => 0, failed => 0}, Rows),
            z_db:q("update import_feed_item i set status='deleted', checksum=null, modified=now()
                where feed_id=$1 and exists (select 1 from import_feed_resource r
                    where r.uri=i.uri and r.rsc_id is null)", [Id], C),
            {ok, Counts};
        {_Token} -> {error, feed_is_running}
    end.

%% @doc Remove a feed, optionally deleting its content with current-user ACLs.
%% Global resource identities remain available for deduplication and tombstones.
-spec delete_feed(Id, DeleteContent, Context) -> {ok, map()} | {error, term()} when
    Id :: term(), DeleteContent :: boolean(), Context :: z:context().
delete_feed(Id, DeleteContent, C) when is_boolean(DeleteContent) ->
    case writable(C) of
        false -> {error, eacces};
        true ->
            Result = z_db:transaction(fun(Tx) ->
                FeedId = integer(Id),
                case z_db:q_row("select lease_token from import_feed where id=$1 for update", [FeedId], Tx) of
                    undefined -> {error, enoent};
                    {undefined} ->
                        ContentResult = case DeleteContent of
                            true -> delete_content_locked(FeedId, Tx);
                            false -> {ok, #{deleted => 0, skipped => 0, failed => 0}}
                        end,
                        case ContentResult of
                            {ok, Counts} ->
                                {ok, 1} = z_db:delete(import_feed, FeedId, Tx),
                                {ok, Counts};
                            Error -> Error
                        end;
                    {_Token} -> {error, feed_is_running}
                end
            end, C),
            case Result of
                {rollback, Reason} -> {error, Reason};
                _ -> Result
            end
    end.

%% Deleting edges can schedule sudo deletion of orphaned dependent resources.
%% Preserve the parent too if that could indirectly delete a forbidden resource.
deletable_tree(Id, Seen, C) ->
    case maps:is_key(Id, Seen) of
        true -> true;
        false ->
            m_rsc:p_no_acl(Id, is_protected, C) =/= true
                andalso m_rsc:is_deletable(Id, C)
                andalso lists:all(fun(Object) ->
                    m_rsc:p_no_acl(Object, is_dependent, C) =/= true
                        orelse m_rsc:p_no_acl(Object, is_protected, C) =:= true
                        orelse deletable_tree(Object, Seen#{Id => true}, C)
                end, m_edge:objects(Id, C))
    end.

-spec decode(map()) -> map().
decode(R) -> R#{connections => binary_to_term(maps:get(connections, R), [safe]),
    user_groups => binary_to_term(maps:get(user_groups, R), [safe])}.
public(R) -> (maps:without([user_groups, lease_token], R))#{is_running => maps:get(lease_token, R, undefined) =/= undefined}.
integer(I) when is_integer(I), I > 0 -> I;
integer(B) when is_binary(B) -> try integer(binary_to_integer(B)) catch _:_ -> undefined end;
integer(_) -> undefined.
bool(K, P) -> z_convert:to_bool(maps:get(K, P, false)).

-spec install(z:context()) -> ok.
install(C) ->
    case z_db:table_exists(import_feed, C) of
        false -> z_db:q("create table import_feed (
        id serial primary key, url varchar(2048) not null unique, title text not null,
        category_id integer references rsc(id) on delete set null,
        content_group_id integer references rsc(id) on delete set null,
        rights text, rights_attribution text, rights_year text,
        remove_patterns text not null default '',
        guess_category boolean not null default false, connections bytea not null,
        media_mode varchar(16) not null default 'dependent' check (media_mode in ('dependent','attached','none')),
        poll_minutes integer not null default 60 check (poll_minutes between 5 and 10080),
        is_authoritative boolean not null default false, use_wordpress boolean not null default true,
        import_depth integer not null default 1 check (import_depth between 0 and 10),
        is_zotonic_feed boolean,
        use_credentials boolean not null default false,
        is_subscribe boolean not null default false,
        is_subscribe_connections boolean not null default false,
        is_paused boolean not null default false,
        user_id integer references rsc(id) on delete set null, user_groups bytea not null,
        next_poll timestamptz not null default now(), lease_until timestamptz,
        lease_token varchar(64), last_poll timestamptz, last_success timestamptz,
        polls bigint not null default 0, imported bigint not null default 0,
        updated bigint not null default 0, skipped bigint not null default 0,
        errors bigint not null default 0, last_error text,
        created timestamptz not null default now(), modified timestamptz not null default now()
    )", C);
        true ->
            z_db:q("alter table import_feed
                add column if not exists rights text,
                add column if not exists rights_attribution text,
                add column if not exists rights_year text,
                add column if not exists remove_patterns text not null default '',
                add column if not exists import_depth integer not null default 1 check (import_depth between 0 and 10),
                add column if not exists is_zotonic_feed boolean,
                add column if not exists use_credentials boolean not null default false,
                add column if not exists is_subscribe boolean not null default false,
                add column if not exists is_subscribe_connections boolean not null default false", C)
    end,
    z_db:q("create index if not exists import_feed_due_key on import_feed(next_poll) where not is_paused", C),
    z_db:q("create table if not exists import_feed_resource (
        uri varchar(2048) primary key,
        rsc_id integer references rsc(id) on delete set null,
        created timestamptz not null default now()
    )", C),
    z_db:q("create index if not exists import_feed_resource_rsc_key on import_feed_resource(rsc_id)", C),
    z_db:q("create table if not exists import_feed_item (
        feed_id integer not null references import_feed(id) on delete cascade,
        uri varchar(2048) not null, kind varchar(16) not null default 'article',
        checksum bytea, status varchar(16) not null, last_error text,
        created timestamptz not null default now(), modified timestamptz not null default now(),
        primary key(feed_id, uri)
    )", C),
    z_db:flush(C),
    ok.

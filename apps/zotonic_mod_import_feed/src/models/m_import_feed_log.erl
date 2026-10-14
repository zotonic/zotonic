%% @copyright 2026 Marc Worrell

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

-module(m_import_feed_log).
-moduledoc(#{zotonic_keywords => ["reference", "operator", "model", "logging_and_monitoring", "import"]}).
-moduledoc("""
Persistent feed activity, accessible to users allowed to use `mod_import_feed`.
`m.import_feed_log.list[FeedId][BeforeId]` returns up to 100 events older than the cursor (0 starts at the newest); use
`all` for all feeds. Invalid filters return an error, never an unfiltered log.
Events retain user, category and content-group IDs as snapshots. History is
removed with its feed or after 30 days. The MQTT topic
`model/import_feed/event/log` carries committed log IDs. Clients fetch and prepend only the new row.
""").
-behaviour(zotonic_model).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([m_get/3, list/3, get/2, write/3, changed/2, install/1, cleanup/1]).
-define(PAGE_SIZE, 100).
-define(RETENTION_DAYS, 30).
-define(MAX_MESSAGE_LENGTH, 2000).

m_get([<<"get">>, Id | Rest], _, Context) ->
    case get(Id, Context) of
        {ok, Row} -> {ok, {Row, Rest}};
        Error -> Error
    end;
m_get([<<"list">>, Feed, Offset | Rest], _, Context) ->
    case list(Feed, Offset, Context) of
        {ok, Rows} -> {ok, {Rows, Rest}};
        Error -> Error
    end;
m_get(_, _, _) -> {error, unknown_path}.

-spec get(Id, Context) -> {ok, map()} | {error, term()} when
    Id :: term(), Context :: z:context().
get(Id, Context) ->
    case m_import_feed:allowed(Context) of
        false -> {error, eacces};
        true ->
            try
                LogId = z_convert:to_integer(Id),
                true = is_integer(LogId) andalso LogId > 0,
                case z_db:qmap("select l.*, f.title as feed_title from import_feed_log l
                        join import_feed f on f.id=l.feed_id where l.id=$1",
                        [LogId], [{keys, atom}], Context) of
                    {ok, [Row]} -> {ok, Row};
                    {ok, []} -> {error, enoent};
                    Error -> Error
                end
            catch error:_ -> {error, enoent} end
    end.

-spec list(Feed, Before, Context) -> {ok, map()} | {error, term()} when
    Feed :: term(), Before :: term(), Context :: z:context().
list(Feed, Before, Context) ->
    case m_import_feed:allowed(Context) of
        false -> {error, eacces};
        true ->
            try
                Cursor = z_convert:to_integer(Before),
                true = is_integer(Cursor) andalso Cursor >= 0,
                {Where, Args} = case Feed of
                    <<"all">> -> {"", [Cursor]};
                    _ ->
                        FeedId = z_convert:to_integer(Feed),
                        true = is_integer(FeedId) andalso FeedId > 0,
                        {" and l.feed_id=$2", [Cursor, FeedId]}
                end,
                {ok, Rows} = z_db:qmap("select l.*, f.title as feed_title from import_feed_log l
                    join import_feed f on f.id=l.feed_id where ($1::bigint=0 or l.id<$1)" ++ Where ++
                    " order by l.id desc limit " ++ integer_to_list(?PAGE_SIZE + 1),
                    Args, [{keys, atom}], Context),
                Items = lists:sublist(Rows, ?PAGE_SIZE),
                Next = case Items of [] -> Cursor; _ -> maps:get(id, lists:last(Items)) end,
                {ok, #{items => Items, next => Next, has_next => length(Rows) > ?PAGE_SIZE}}
            catch error:_ -> {error, invalid_filter} end
    end.

%% Internal writes use the worker's context and participate in its transaction.
%% Only explicit scalar fields are persisted: no feed bodies or credentials.
-spec write(FeedId, Event, Context) -> ok when
    FeedId :: integer(), Event :: map(), Context :: z:context().
write(FeedId, Event, Context) ->
    Props = maps:with([event, status, uri, user_id, rsc_id, category_id,
        content_group_id, message], Event),
    Message = case maps:get(message, Props, undefined) of
        undefined -> undefined;
        Text -> z_string:truncate(Text, ?MAX_MESSAGE_LENGTH)
    end,
    {ok, LogId} = z_db:insert(import_feed_log, Props#{feed_id => FeedId, message => Message}, Context),
    %% The notifier queues this until transaction commit and drops it on rollback.
    z_notifier:notify({import_feed_log, FeedId, LogId}, Context),
    ok.

-spec changed(FeedId, Context) -> ok when FeedId :: integer(), Context :: z:context().
changed(FeedId, Context) ->
    _ = z_mqtt:publish(<<"model/import_feed/event/log">>, #{feed_id => FeedId}, Context),
    ok.

-spec cleanup(Context) -> ok when Context :: z:context().
cleanup(Context) ->
    z_db:q("delete from import_feed_log where created < now() - $1 * interval '1 day'",
        [?RETENTION_DAYS], Context),
    ok.

-spec install(Context) -> ok when Context :: z:context().
install(Context) ->
    z_db:q("create table if not exists import_feed_log (
        id bigserial primary key,
        feed_id integer not null references import_feed(id) on delete cascade,
        event varchar(32) not null, status varchar(32) not null,
        uri text, user_id integer, rsc_id integer, category_id integer,
        content_group_id integer, message text,
        created timestamptz not null default clock_timestamp()
    )", Context),
    z_db:q("create index if not exists import_feed_log_feed_id_key on import_feed_log(feed_id, id desc)", Context),
    z_db:q("create index if not exists import_feed_log_created_key on import_feed_log(created)", Context),
    ok.

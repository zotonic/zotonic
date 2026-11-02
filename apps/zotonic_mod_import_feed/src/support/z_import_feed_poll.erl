%% @copyright 2026 Marc Worrell
%% @doc Durable due dates and leases; transient per-site sidejob does bounded work.
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

-module(z_import_feed_poll).
-define(FEEDS_PER_RUN, 5).
-define(LEASE_TOKEN_BYTES, 24).
-define(PAGES_PER_POLL, 100).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1, sort_entries/1]).

-spec run(z:context()) -> ok.
run(Context) -> run(?FEEDS_PER_RUN, Context).
run(0, _) -> ok;
run(FeedsRemaining, Context) ->
    Token = binary:encode_hex(crypto:strong_rand_bytes(?LEASE_TOKEN_BYTES)),
    %% SKIP LOCKED permits multiple Zotonic nodes without duplicate claims.
    %% Capture and consume the restore request atomically with the lease. It
    %% belongs to this fetch only, even if the worker or network subsequently fails.
    %% Keep the settings timestamp as text: datetime decoding loses fractional seconds.
    {ok, Rows} = z_db:qmap("
        with due as (
            select id, refetch_deleted
            from import_feed
            where (not is_paused or refetch_deleted) and next_poll<=now()
              and (lease_until is null or lease_until<now())
            order by next_poll
            for update skip locked limit 1
        ) update import_feed f
            set lease_token=$1,
                lease_until=now()+interval '15 minutes',
                last_poll=now(),
                polls=polls+1,
                refetch_deleted=false
            from due
            where f.id=due.id
            returning f.*, f.modified::text as settings_version, due.refetch_deleted as refetch_deleted_once",
        [Token], [{keys, atom}], Context),
    case Rows of
        [] -> ok;
        [Row] ->
            FeedSettings = m_import_feed:decode(Row),
            Message = case maps:get(refetch_deleted_once, FeedSettings, false) of
                true -> <<"Fetching feed, including deleted resources">>;
                false -> <<"Fetching feed">>
            end,
            poll_log(FeedSettings, <<"started">>, Message, Context),
            Result = try
                poll(FeedSettings, Context)
            catch
                Class:Reason:Stack ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_import_feed,
                        text => <<"Feed poll failed">>,
                        feed_id => maps:get(id, FeedSettings),
                        result => error,
                        reason => {Class, Reason},
                        stack => Stack
                    }),
                    {error, poll_failed}
            end,
            finish(FeedSettings, Result, Context),
            run(FeedsRemaining - 1, Context)
    end.

poll(#{user_id := User} = FeedSettings, Context) when is_integer(User) ->
    case m_import_feed:is_admin_user(User, Context) of
        true ->
            {error, admin_import_user};
        false ->
            poll_user(FeedSettings, Context)
    end;
poll(_, _) ->
    {error, missing_editor}.

poll_user(#{user_id := User} = FeedSettings, Context) ->
    case z_auth:is_enabled(User, Context) of
        true ->
            ImportContext = z_acl:logon(User, #{user_groups => maps:get(user_groups, FeedSettings)}, z_context:new(Context)),
            case z_acl:is_read_only(ImportContext) of
                false ->
                    fetch(FeedSettings, z_import_feed_http:context(FeedSettings, ImportContext));
                true ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_import_feed,
                        text => <<"Could not import feed, access permissions of user are set to read only.">>,
                        result => error,
                        reason => eacces,
                        user => User,
                        feed_url => maps:get(url, FeedSettings, <<"-">>),
                        feed_id => maps:get(id, FeedSettings, undefined)
                    }),
                    {error, eacces}
            end;
        false ->
            {error, editor_disabled}
    end.

fetch(FeedSettings, Context) ->
    %% Keep the boundary across retries, so entries imported by this traversal
    %% do not become a reason to stop before a failed later page is retried.
    Started = z_db:q1("update import_feed
        set pagination_started=coalesce(pagination_started, now())
        where id=$1 and lease_token=$2 and modified::text=$3
        returning pagination_started::text",
        [maps:get(id, FeedSettings), maps:get(lease_token, FeedSettings),
         maps:get(settings_version, FeedSettings)], Context),
    case Started of
        undefined -> {error, cancelled};
        _ -> fetch_first(FeedSettings#{pagination_version => Started}, Context)
    end.

fetch_first(FeedSettings, Context) ->
    Response = case maps:get(next_url, FeedSettings, undefined) of
        Url when is_binary(Url), Url =/= <<>> -> z_import_feed_http:get(Url, Context);
        _ -> z_import_feed_http:get_feed(FeedSettings, Context)
    end,
    case Response of
        {not_modified, {FinalUrl, Headers}} ->
            Validators = maps:merge(
                maps:with([http_etag, http_last_modified], FeedSettings),
                z_import_feed_http:validators(Headers)),
            poll_log(FeedSettings, <<"unchanged">>, <<"Feed not modified (HTTP 304)">>, Context),
            {ok, #{imported => 0, updated => 0, skipped => 0, errors => 0,
                last_error => undefined,
                publisher_seconds => maps:get(poll_hint_seconds, FeedSettings, 0),
                http_validators => Validators#{http_url => z_convert:to_binary(FinalUrl)}}};
        {ok, {FinalUrl, Headers, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(FinalUrl)) of
                {ok, #{truncated := true}} -> {error, too_many_entries};
                {ok, ParsedFeed} ->
                    m_import_feed:record_feed_type(FeedSettings, ParsedFeed, Context),
                    poll_log(
                        FeedSettings, <<"parsed">>,
                        iolist_to_binary(io_lib:format("Parsed ~B entries", [length(maps:get(entries, ParsedFeed))])), Context),
                    Validators = case maps:get(next_url, FeedSettings, undefined) of
                        undefined -> (z_import_feed_http:validators(Headers))#{http_url => z_convert:to_binary(FinalUrl)};
                        _ -> #{}
                    end,
                    Counts = #{
                        http_validators => Validators,
                        imported => 0,
                        updated => 0,
                        skipped => 0,
                        errors => 0,
                        last_error => undefined,
                        delay_seconds => maps:get(refresh_seconds, ParsedFeed, 0),
                        publisher_seconds => maps:get(refresh_seconds, ParsedFeed, 0)
                    },
                    ImportSettings = restore_settings(FeedSettings#{feed => maps:without([entries], ParsedFeed)}, Context),
                    page(ParsedFeed, ImportSettings, Counts, ?PAGES_PER_POLL,
                        [z_convert:to_binary(FinalUrl)], Context);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% Check prior successful entries before importing this page (which changes
%% their timestamps). Always process the whole page, including updated entries.
page(Parsed, Settings, Counts, Remaining, Visited, Context) ->
    Entries = maps:get(entries, Parsed),
    Uris = [U || #{uri := U} <- Entries, is_binary(U)],
    Seen = z_db:q1("select exists(select 1 from import_feed_item
        where feed_id=$1 and uri=any($2::text[]) and status in ('ok','deleted')
          and modified < $3::text::timestamptz)",
        [maps:get(id, Settings), Uris, maps:get(pagination_version, Settings)], Context),
    Counts1 = entries(sort_entries(Entries), Settings#{feed => maps:without([entries], Parsed)}, Counts, Context),
    Initial = maps:get(last_success, Settings, undefined) =:= undefined,
    Follow = (not Initial orelse maps:get(import_all_pages, Settings, false))
        andalso not Seen,
    Next = case Follow of true -> maps:get(next_url, Parsed, <<>>); false -> <<>> end,
    case {maps:get(errors, Counts1), Next} of
        {0, <<>>} -> {ok, Counts1};
        {0, Url} when is_binary(Url) ->
            case lists:member(Url, Visited) of
                true -> page_error(pagination_loop, Counts1);
                false ->
                    case checkpoint(Url, Settings, Context) of
                        false -> page_error(cancelled, Counts1);
                        true when Remaining =< 1 ->
                            {ok, Counts1#{pagination_pending => true, http_validators => #{}}};
                        true -> next_page(Url, Settings, Counts1#{http_validators => #{}},
                            Remaining - 1, [Url | Visited], Context)
                    end
            end;
        _ -> {ok, Counts1}
    end.

checkpoint(Url, Settings, Context) ->
    z_db:q1("update import_feed set next_url=$4, lease_until=now()+interval '15 minutes'
        where id=$1 and lease_token=$2 and modified::text=$3
          and (not is_paused or $5) returning id",
        [maps:get(id, Settings), maps:get(lease_token, Settings),
         maps:get(settings_version, Settings), Url,
         maps:get(refetch_deleted_once, Settings, false)], Context) =/= undefined.

next_page(Url, Settings, Counts, Remaining, Visited, Context) ->
    %% No first-page validators on continuation requests. The same URL checks,
    %% response limits and destination credential policy apply to every fetch.
    case z_import_feed_http:get(Url, Context) of
        {ok, {FinalUrl, _, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(FinalUrl)) of
                {ok, #{truncated := true}} -> page_error(too_many_entries, Counts);
                {ok, Parsed} -> page(Parsed, Settings, Counts, Remaining,
                    lists:usort([z_convert:to_binary(FinalUrl) | Visited]), Context);
                {error, Reason} -> page_error(Reason, Counts)
            end;
        {error, Reason} -> page_error(Reason, Counts)
    end.

page_error(Reason, Counts) ->
    {ok, Counts#{errors := maps:get(errors, Counts) + 1, last_error := Reason,
        delay_seconds => max(maps:get(delay_seconds, Counts, 0), z_import_feed_http:error_delay(Reason))}}.

%% Keep the initial deletion set for this fetch. Several articles can embed the
%% same deleted medium; all must update their embeds after the first restores it.
restore_settings(#{refetch_deleted_once := true, id := FeedId} = Feed, Context) ->
    Rows = z_db:q("
        select r.uri
        from import_feed_resource r
            join import_feed_item i on i.uri=r.uri
        where i.feed_id=$1
          and r.rsc_id is null", [FeedId], Context),
    Feed#{refetch_deleted_uris => maps:from_list([{Uri, true} || {Uri} <- Rows])};
restore_settings(Feed, _) -> Feed.

%% @doc Oldest first, with undated entries last. Preserve feed order for equal dates.
-spec sort_entries(Entries) -> [map()] when Entries :: [map()].
sort_entries(Entries) ->
    [Entry || {_, Entry} <- lists:keysort(1, [{entry_date(Entry), Entry} || Entry <- Entries])].

entry_date(#{published := {_, _} = Date}) -> {0, Date};
entry_date(#{modified := {_, _} = Date}) -> {0, Date};
entry_date(_) -> {1, undefined}.

entries([], _, Counts, _) -> Counts;
entries([Entry | Rest], FeedSettings, Counts, Context) ->
    %% Refresh the lease and check pause/configuration changes before every entry.
    Active = z_db:q1("
        update import_feed
        set lease_until=now()+interval '15 minutes'
        where id=$1
          and lease_token=$2
          and (not is_paused or $4)
          and modified::text=$3
        returning id",
        [
            maps:get(id, FeedSettings),
            maps:get(lease_token, FeedSettings),
            maps:get(settings_version, FeedSettings),
            maps:get(refetch_deleted_once, FeedSettings, false)
        ],
        Context),
    case Active of
        undefined -> Counts#{errors := maps:get(errors, Counts) + 1, last_error := cancelled};
        _ ->
            case m_import_feed:is_admin_user(maps:get(user_id, FeedSettings), Context) of
                true ->
                    Counts#{
                        errors := maps:get(errors, Counts) + 1,
                        last_error := admin_import_user
                    };
                false ->
                    import_entry(Entry, Rest, FeedSettings, Counts, Context)
            end
    end.

import_entry(SourceEntry, Rest, FeedSettings, Counts, Context) ->
    Entry = case maps:get(use_wordpress, FeedSettings) of
        true -> z_import_feed_wordpress:enrich(SourceEntry, Context);
        false -> SourceEntry
    end,
    Result = try
        z_import_feed_import:run(Entry, FeedSettings, Context)
    catch _:_ ->
        {error, entry_failed}
    end,
    NextCounts = case Result of
        Status
            when Status =:= imported;
                 Status =:= updated;
                 Status =:= skipped ->
            Counts#{
                Status := maps:get(Status, Counts) + 1
            };
        {error, Reason} ->
            NewDelay = max(maps:get(delay_seconds, Counts, 0), z_import_feed_http:error_delay(Reason)),
            Counts#{
                errors := maps:get(errors, Counts) + 1,
                last_error := Reason,
                delay_seconds => NewDelay
            }
    end,
    entries(Rest, FeedSettings, NextCounts, Context).

finish(FeedSettings, {error, Reason}, Context) ->
    Result = #{
        imported => 0,
        updated => 0,
        skipped => 0,
        errors => 1,
        last_error => Reason,
        delay_seconds => z_import_feed_http:error_delay(Reason),
        poll_failed => true
    },
    finish(FeedSettings, {ok, Result}, Context);
finish(FeedSettings, {ok, Counts}, Context) ->
    Status = case maps:get(errors, Counts) of
        0 -> <<"ok">>;
        _ -> <<"error">>
    end,
    Summary = iolist_to_binary(
            io_lib:format("Imported: ~B; updated: ~B; skipped: ~B; errors: ~B. ~ts", [
                maps:get(imported, Counts),
                maps:get(updated, Counts),
                maps:get(skipped, Counts),
                maps:get(errors, Counts),
                case maps:get(last_error, Counts) of
                    undefined -> <<>>;
                    LastError -> m_import_feed:error_message(LastError, Context)
                end])),
    Error = case maps:get(last_error, Counts) of
        undefined -> undefined;
        Reason -> m_import_feed:error_message(Reason, Context)
    end,
    Failed = maps:get(poll_failed, Counts, false),
    PublisherSeconds = case Failed of
        true -> maps:get(poll_hint_seconds, FeedSettings, 0);
        false -> maps:get(publisher_seconds, Counts, 0)
    end,
    Backoff = case Failed of
        true ->
            min(
                12 * 3600,
                maps:get(poll_minutes, FeedSettings) * 60
                * (1 bsl min(maps:get(failure_count, FeedSettings, 0) + 1, 16)));
        false ->
            0
    end,
    Delay = lists:max([PublisherSeconds, Backoff, maps:get(delay_seconds, Counts, 0)]),
    Validators = case maps:get(errors, Counts) of
        0 -> maps:get(http_validators, Counts, #{});
        _ -> #{}
    end,
    Complete = maps:get(errors, Counts) =:= 0 andalso not maps:get(pagination_pending, Counts, false),
    z_db:q("
        update import_feed
        set next_url=case when $15 and modified::text=$8 then null else next_url end,
            pagination_started=case when $15 and modified::text=$8 then null else pagination_started end,
            http_etag=case when modified::text=$8 then $12 else null end,
            http_last_modified=case when modified::text=$8 then $13 else null end,
            http_url=case when modified::text=$8 then $14 else null end,
            http_settings_version=case when modified::text=$8 and $5=0 then $8 else null end,
            lease_token=null,
            lease_until=null,
            next_poll=case when modified::text=$8 and not refetch_deleted
                      then now()+(case when $16 then 1 else greatest(poll_minutes*60, $9::bigint) end)*interval '1 second'
                      else next_poll end,
            failure_count=case when $10 then least(failure_count+1, 31) else 0 end,
            poll_hint_seconds=$11,
            imported=imported+$2,
            updated=updated+$3,
            skipped=skipped+$4,
            errors=errors+$5,
            last_error=$6,
            last_success=case when $15 and modified::text=$8 then now() else last_success end
        where id=$1
          and lease_token=$7",
        [
            maps:get(id, FeedSettings), maps:get(imported, Counts), maps:get(updated, Counts),
            maps:get(skipped, Counts), maps:get(errors, Counts), Error,
            maps:get(lease_token, FeedSettings), maps:get(settings_version, FeedSettings),
            Delay, Failed, PublisherSeconds,
            maps:get(http_etag, Validators, undefined),
            maps:get(http_last_modified, Validators, undefined),
            maps:get(http_url, Validators, undefined), Complete,
            maps:get(pagination_pending, Counts, false)
        ],
        Context),
    poll_log(FeedSettings, Status, Summary, Context).

poll_log(Feed, Status, Message, Context) ->
    FeedId = maps:get(id, Feed),
    ok = m_import_feed_log:write(FeedId, #{
        event => <<"poll">>,
        status => Status,
        uri => maps:get(url, Feed, undefined),
        user_id => maps:get(user_id, Feed, undefined),
        category_id => maps:get(category_id, Feed, undefined),
        content_group_id => maps:get(content_group_id, Feed, undefined),
        message => Message
    }, Context),
    m_import_feed_log:changed(FeedId, Context).

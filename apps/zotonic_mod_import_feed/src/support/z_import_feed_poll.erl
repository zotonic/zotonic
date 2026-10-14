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
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1, sort_entries/1]).

-spec run(z:context()) -> ok.
run(Context) -> run(?FEEDS_PER_RUN, Context).
run(0, _) -> ok;
run(FeedsRemaining, Context) ->
    Token = binary:encode_hex(crypto:strong_rand_bytes(?LEASE_TOKEN_BYTES)),
    %% SKIP LOCKED permits multiple Zotonic nodes without duplicate claims.
    {ok, Rows} = z_db:qmap("update import_feed set lease_token=$1,
        lease_until=now()+interval '15 minutes', last_poll=now(), polls=polls+1
        where id=(select id from import_feed where not is_paused and next_poll<=now()
            and (lease_until is null or lease_until<now()) order by next_poll
            for update skip locked limit 1) returning *", [Token], [{keys, atom}], Context),
    case Rows of
        [] -> ok;
        [Row] ->
            FeedSettings = m_import_feed:decode(Row),
            poll_log(FeedSettings, <<"started">>, <<"Fetching feed">>, Context),
            Result = try
                         poll(FeedSettings, Context)
                     catch
                         Class:Reason:Stack ->
                             ?LOG_ERROR(#{in => zotonic_mod_import_feed, text => <<"Feed poll failed">>,
                                          feed_id => maps:get(id, FeedSettings), result => error, reason => {Class, Reason}, stack => Stack}),
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
                    ?LOG_WARNING( #{ text => <<"Could not import feed, access permissions of user are set to read only.">>,
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
    case z_import_feed_http:get(maps:get(url, FeedSettings), Context) of
        {ok, {FinalUrl, _, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(FinalUrl)) of
                {ok, #{truncated := true}} -> {error, too_many_entries};
                {ok, ParsedFeed} ->
                    m_import_feed:record_feed_type(FeedSettings, ParsedFeed, Context),
                    poll_log(FeedSettings, <<"parsed">>,
                        iolist_to_binary(io_lib:format("Parsed ~B entries", [length(maps:get(entries, ParsedFeed))])), Context),
                    Counts = #{imported => 0, updated => 0, skipped => 0, errors => 0, last_error => undefined},
                    {ok, entries(sort_entries(maps:get(entries, ParsedFeed)), FeedSettings#{feed => maps:without([entries], ParsedFeed)}, Counts, Context)};
                Error -> Error
            end;
        Error -> Error
    end.

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
    Active = z_db:q1("update import_feed set lease_until=now()+interval '15 minutes'
        where id=$1 and lease_token=$2 and not is_paused and modified=$3 returning id",
        [maps:get(id, FeedSettings), maps:get(lease_token, FeedSettings), maps:get(modified, FeedSettings)], Context),
    case Active of
        undefined -> Counts;
        _ ->
            case m_import_feed:is_admin_user(maps:get(user_id, FeedSettings), Context) of
                true -> Counts#{errors := maps:get(errors, Counts) + 1, last_error := admin_import_user};
                false -> import_entry(Entry, Rest, FeedSettings, Counts, Context)
            end
    end.

import_entry(SourceEntry, Rest, FeedSettings, Counts, Context) ->
    Entry = case maps:get(use_wordpress, FeedSettings) of
        true -> z_import_feed_wordpress:enrich(SourceEntry, Context);
        false -> SourceEntry
    end,
    Result = try z_import_feed_import:run(Entry, FeedSettings, Context)
        catch _:_ -> {error, entry_failed} end,
    NextCounts = case Result of
        Status when Status =:= imported; Status =:= updated; Status =:= skipped ->
            Counts#{Status := maps:get(Status, Counts) + 1};
        {error, Reason} -> Counts#{errors := maps:get(errors, Counts) + 1, last_error := Reason}
    end,
    entries(Rest, FeedSettings, NextCounts, Context).

finish(FeedSettings, {error, Reason}, Context) ->
    finish(FeedSettings, {ok, #{imported => 0, updated => 0, skipped => 0, errors => 1, last_error => Reason}}, Context);
finish(FeedSettings, {ok, Counts}, Context) ->
    Status = case maps:get(errors, Counts) of 0 -> <<"ok">>; _ -> <<"error">> end,
    Summary = iolist_to_binary(io_lib:format("Imported: ~B; updated: ~B; skipped: ~B; errors: ~B. ~ts", [
        maps:get(imported, Counts), maps:get(updated, Counts), maps:get(skipped, Counts), maps:get(errors, Counts),
        case maps:get(last_error, Counts) of undefined -> <<>>; LastError -> m_import_feed:error_message(LastError, Context) end])),
    Error = case maps:get(last_error, Counts) of
        undefined -> undefined;
        Reason -> m_import_feed:error_message(Reason, Context)
    end,
    z_db:q("update import_feed set lease_token=null, lease_until=null,
        next_poll=case when modified=$8 then now()+poll_minutes*interval '1 minute' else next_poll end,
        imported=imported+$2, updated=updated+$3, skipped=skipped+$4, errors=errors+$5,
        last_error=$6, last_success=case when $5=0 then now() else last_success end
        where id=$1 and lease_token=$7",
        [maps:get(id, FeedSettings), maps:get(imported, Counts), maps:get(updated, Counts),
         maps:get(skipped, Counts), maps:get(errors, Counts), Error,
         maps:get(lease_token, FeedSettings), maps:get(modified, FeedSettings)], Context),
    poll_log(FeedSettings, Status, Summary, Context).

poll_log(Feed, Status, Message, Context) ->
    FeedId = maps:get(id, Feed),
    ok = m_import_feed_log:write(FeedId, #{event => <<"poll">>, status => Status,
        uri => maps:get(url, Feed, undefined), user_id => maps:get(user_id, Feed, undefined),
        category_id => maps:get(category_id, Feed, undefined),
        content_group_id => maps:get(content_group_id, Feed, undefined), message => Message}, Context),
    m_import_feed_log:changed(FeedId, Context).

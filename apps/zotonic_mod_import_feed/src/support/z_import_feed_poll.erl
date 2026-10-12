%% @copyright 2026 Marc Worrell
%% @doc Durable due dates and leases; transient per-site sidejob does bounded work.

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
run(C) -> run(?FEEDS_PER_RUN, C).
run(0, _) -> ok;
run(N, C) ->
    Token = binary:encode_hex(crypto:strong_rand_bytes(?LEASE_TOKEN_BYTES)),
    %% SKIP LOCKED permits multiple Zotonic nodes without duplicate claims.
    {ok, Rows} = z_db:qmap("update import_feed set lease_token=$1,
        lease_until=now()+interval '15 minutes', last_poll=now(), polls=polls+1
        where id=(select id from import_feed where not is_paused and next_poll<=now()
            and (lease_until is null or lease_until<now()) order by next_poll
            for update skip locked limit 1) returning *", [Token], [{keys, atom}], C),
    case Rows of
        [] -> ok;
        [Row] ->
            F = m_import_feed:decode(Row),
            Result = try
                         poll(F, C)
                     catch
                         Class:Reason:Stack ->
                             ?LOG_ERROR(#{in => zotonic_mod_import_feed, text => <<"Feed poll failed">>,
                                          feed_id => maps:get(id, F), result => error, reason => {Class, Reason}, stack => Stack}),
                             {error, poll_failed}
            end,
            finish(F, Result, C),
            run(N - 1, C)
    end.

poll(#{user_id := User} = F, C) when is_integer(User) ->
    case m_import_feed:is_admin_user(User, C) of
        true ->
            {error, admin_import_user};
        false ->
            poll_user(F, C)
    end;
poll(_, _) ->
    {error, missing_editor}.

poll_user(#{user_id := User} = F, Context) ->
    case z_auth:is_enabled(User, Context) of
        true ->
            Editor = z_acl:logon(User, #{user_groups => maps:get(user_groups, F)}, z_context:new(Context)),
            IsImportFeedAllowed = m_import_feed:allowed(Editor),
            IsReadOnly = z_acl:is_read_only(Editor),
            case IsImportFeedAllowed andalso not IsReadOnly of
                true ->
                    fetch(F, z_import_feed_http:context(F, Editor));
                false ->
                    ?LOG_WARNING( #{ text => <<"Could not import feed">>,
                                     user => User,
                                     is_import_allowed => IsImportFeedAllowed,
                                     is_read_only => IsReadOnly,
                                     feed_url => maps:get(url, F, <<"-">>),
                                     feed_id => maps:get(id, F, undefined)
                                   }),
                    {error, eacces}
            end;
        false ->
            {error, editor_disabled}
    end.

fetch(F, C) ->
    case z_import_feed_http:get(maps:get(url, F), C) of
        {ok, {Final, _, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(Final)) of
                {ok, #{truncated := true}} -> {error, too_many_entries};
                {ok, Feed} ->
                    m_import_feed:record_feed_type(F, Feed, C),
                    Counts = #{imported => 0, updated => 0, skipped => 0, errors => 0, last_error => undefined},
                    {ok, entries(sort_entries(maps:get(entries, Feed)), F#{feed => maps:without([entries], Feed)}, Counts, C)};
                E -> E
            end;
        E -> E
    end.

%% @doc Oldest first, with undated entries last. Preserve feed order for equal dates.
-spec sort_entries(Entries) -> [map()] when Entries :: [map()].
sort_entries(Entries) ->
    [E || {_, E} <- lists:keysort(1, [{entry_date(E), E} || E <- Entries])].

entry_date(#{published := {_, _} = Date}) -> {0, Date};
entry_date(#{modified := {_, _} = Date}) -> {0, Date};
entry_date(_) -> {1, undefined}.

entries([], _, Counts, _) -> Counts;
entries([E | Rest], F, Counts, C) ->
    %% Refresh the lease and check pause/configuration changes before every entry.
    Active = z_db:q1("update import_feed set lease_until=now()+interval '15 minutes'
        where id=$1 and lease_token=$2 and not is_paused and modified=$3 returning id",
        [maps:get(id, F), maps:get(lease_token, F), maps:get(modified, F)], C),
    case Active of
        undefined -> Counts;
        _ ->
            case m_import_feed:is_admin_user(maps:get(user_id, F), C) of
                true -> Counts#{errors := maps:get(errors, Counts) + 1, last_error := admin_import_user};
                false -> import_entry(E, Rest, F, Counts, C)
            end
    end.

import_entry(E, Rest, F, Counts, C) ->
    Entry = case maps:get(use_wordpress, F) of
        true -> z_import_feed_wordpress:enrich(E, C);
        false -> E
    end,
    Result = try z_import_feed_import:run(Entry, F, C)
        catch _:_ -> {error, entry_failed} end,
    Next = case Result of
        K when K =:= imported; K =:= updated; K =:= skipped ->
            Counts#{K := maps:get(K, Counts) + 1};
        {error, Reason} -> Counts#{errors := maps:get(errors, Counts) + 1, last_error := Reason}
    end,
    entries(Rest, F, Next, C).

finish(F, {error, Reason}, C) ->
    finish(F, {ok, #{imported => 0, updated => 0, skipped => 0, errors => 1, last_error => Reason}}, C);
finish(F, {ok, Counts}, C) ->
    Error = case maps:get(last_error, Counts) of
        undefined -> undefined;
        Reason -> m_import_feed:error_message(Reason, C)
    end,
    z_db:q("update import_feed set lease_token=null, lease_until=null,
        next_poll=case when modified=$8 then now()+poll_minutes*interval '1 minute' else next_poll end,
        imported=imported+$2, updated=updated+$3, skipped=skipped+$4, errors=errors+$5,
        last_error=$6, last_success=case when $5=0 then now() else last_success end
        where id=$1 and lease_token=$7",
        [maps:get(id, F), maps:get(imported, Counts), maps:get(updated, Counts),
         maps:get(skipped, Counts), maps:get(errors, Counts), Error,
         maps:get(lease_token, F), maps:get(modified, F)], C),
    ok.

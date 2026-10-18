%% @copyright 2026 Marc Worrell
%% @doc Manual refresh of a known article, using its feed's normal import pipeline.
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

-module(z_import_feed_fetch).
-export([run/4]).

-define(LEASE_TOKEN_BYTES, 24).

-spec run(FeedId, Id, Uri, Context) -> {ok, integer()} | {error, term()}
    when FeedId :: integer(), Id :: integer(), Uri :: binary(), Context :: z:context().
run(FeedId, Id, Uri, Context) ->
    %% Claim a lease in one statement; never keep a transaction open while
    %% fetching the feed or importing its resources. Deletion checks this lease.
    Token = binary:encode_hex(crypto:strong_rand_bytes(?LEASE_TOKEN_BYTES)),
    try
        case z_db:qmap("update import_feed set lease_token=$2,
                lease_until=now()+interval '15 minutes'
                where id=$1 and (lease_until is null or lease_until<now())
                returning *", [FeedId, Token], [{keys, atom}], Context) of
            {ok, [Row]} ->
                try refresh(m_import_feed:decode(Row), Id, Uri, Context)
                after
                    z_db:q("update import_feed set lease_token=null, lease_until=null
                        where id=$1 and lease_token=$2", [FeedId, Token], Context)
                end;
            {ok, []} ->
                case z_db:q1("select id from import_feed where id=$1", [FeedId], Context) of
                    undefined -> {error, enoent};
                    _ -> {error, feed_is_running}
                end;
            Error -> Error
        end
    after z_memo:set_userid(Context) end.

refresh(#{user_id := User} = FeedSettings, Id, Uri, Context) when is_integer(User) ->
    case {m_import_feed:is_admin_user(User, Context), z_auth:is_enabled(User, Context)} of
        {true, _} -> {error, admin_import_user};
        {false, false} -> {error, editor_disabled};
        {false, true} ->
            ImportContext = z_acl:logon(User, #{user_groups => maps:get(user_groups, FeedSettings)}, Context),
            case m_import_feed:allowed(ImportContext) andalso not z_acl:is_read_only(ImportContext)
                    andalso z_acl:rsc_editable(Id, ImportContext) of
                true -> fetch(FeedSettings, Id, Uri, z_import_feed_http:context(FeedSettings, ImportContext));
                false -> {error, eacces}
            end
    end;
refresh(_, _, _, _) -> {error, missing_editor}.

fetch(FeedSettings, Id, Uri, Context) ->
    case z_import_feed_http:get(maps:get(url, FeedSettings), Context) of
        {ok, {FinalUrl, _, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(FinalUrl)) of
                {ok, #{truncated := true}} -> {error, too_many_entries};
                {ok, ParsedFeed} ->
                    m_import_feed:record_feed_type(FeedSettings, ParsedFeed, Context),
                    case [Entry || #{uri := EntryUri} = Entry <- maps:get(entries, ParsedFeed), EntryUri =:= Uri] of
                        [Entry | _] -> import(enrich(Entry, FeedSettings, Context), FeedSettings#{feed => maps:without([entries], ParsedFeed)}, Id, Context);
                        [] -> {error, feed_entry_not_found}
                    end;
                Error -> Error
            end;
        {error, _} = Error -> Error
    end.

enrich(Entry, #{use_wordpress := true}, Context) -> z_import_feed_wordpress:enrich(Entry, Context);
enrich(Entry, _, _) -> Entry.

import(Entry, FeedSettings, _Id, Context) ->
    case z_import_feed_import:run(Entry, FeedSettings, true, Context) of
        Result when Result =:= updated; Result =:= imported ->
            case z_db:q1("select rsc_id from import_feed_resource where uri=$1", [maps:get(uri, Entry)], Context) of
                Id when is_integer(Id) -> {ok, Id};
                _ -> {error, enoent}
            end;
        skipped -> {error, feed_import_skipped};
        {error, _} = Error -> Error
    end.

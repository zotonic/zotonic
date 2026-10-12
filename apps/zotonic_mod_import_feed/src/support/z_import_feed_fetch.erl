%% @copyright 2026 Marc Worrell
%% @doc Manual refresh of a known article, using its feed's normal import pipeline.

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

-spec run(FeedId, Id, Uri, Context) -> {ok, integer()} | {error, term()}
    when FeedId :: integer(), Id :: integer(), Uri :: binary(), Context :: z:context().
run(FeedId, Id, Uri, Context) ->
    %% Lock the configuration against deletion or changes during the refresh.
    Result = try z_db:transaction(fun(TransactionContext) ->
        case z_db:qmap("select * from import_feed where id=$1 for update",
                [FeedId], [{keys, atom}], TransactionContext) of
            {ok, [Row]} -> refresh(m_import_feed:decode(Row), Id, Uri, TransactionContext);
            {ok, []} -> {error, enoent};
            Error -> Error
        end
    end, Context)
    after z_memo:set_userid(Context) end,
    case Result of
        {rollback, Reason} -> {error, Reason};
        Other -> Other
    end.

refresh(#{user_id := User} = FeedSettings, Id, Uri, Context) when is_integer(User) ->
    case {m_import_feed:is_admin_user(User, Context), z_auth:is_enabled(User, Context)} of
        {true, _} -> {error, admin_import_user};
        {false, false} -> {error, editor_disabled};
        {false, true} ->
            Editor = z_acl:logon(User, #{user_groups => maps:get(user_groups, FeedSettings)}, Context),
            case m_import_feed:allowed(Editor) andalso not z_acl:is_read_only(Editor)
                    andalso z_acl:rsc_editable(Id, Editor) of
                true -> fetch(FeedSettings, Id, Uri, z_import_feed_http:context(FeedSettings, Editor));
                false -> {error, eacces}
            end
    end;
refresh(_, _, _, _) -> {error, missing_editor}.

fetch(FeedSettings, Id, Uri, Context) ->
    case z_import_feed_http:get(maps:get(url, FeedSettings), Context) of
        {ok, {Final, _, _, Body}} ->
            case z_import_feed_parse:parse(Body, z_convert:to_binary(Final)) of
                {ok, #{truncated := true}} -> {error, too_many_entries};
                {ok, Feed} ->
                    m_import_feed:record_feed_type(FeedSettings, Feed, Context),
                    case [Entry || #{uri := EntryUri} = Entry <- maps:get(entries, Feed), EntryUri =:= Uri] of
                        [Entry | _] -> import(enrich(Entry, FeedSettings, Context), FeedSettings#{feed => maps:without([entries], Feed)}, Id, Context);
                        [] -> {error, feed_entry_not_found}
                    end;
                Error -> Error
            end;
        Error -> Error
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
        Error -> Error
    end.

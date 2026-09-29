%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Tests for mailing runs, delivery notifications and recipient data retention.
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

-module(m_mailinglist_run_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([equery/4, squery/3]).

sender_address_test() ->
    Modules = [m_rsc, m_config],
    Context = #context{},
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        lists:foreach(
            fun({ListAddress, ModuleAddress, SiteAddress, Expected}) ->
                meck:expect(m_rsc, p, fun
                    (_, <<"mailinglist_reply_to">>, _) -> ListAddress;
                    (_, <<"mailinglist_sender_name">>, _) -> <<"Newsletter">>
                end),
                meck:expect(m_config, get_value, fun
                    (mod_mailinglist, email_from, _) -> ModuleAddress;
                    (site, email_from, _) -> SiteAddress
                end),
                From = m_mailinglist:get_email_from(1, Context),
                ?assertEqual({<<"Newsletter">>, Expected}, z_email:split_name_email(From))
            end,
            [
                {<<"list@example.com">>, <<"module@example.com">>, <<"site@example.com">>, <<"list@example.com">>},
                {undefined, <<"module@example.com">>, <<"site@example.com">>, <<"module@example.com">>},
                {<<>>, "module@example.com", <<"site@example.com">>, <<"module@example.com">>},
                {undefined, undefined, <<"site@example.com">>, <<"site@example.com">>},
                {<<>>, <<>>, <<"site@example.com">>, <<"site@example.com">>},
                {[], [], <<"site@example.com">>, <<"site@example.com">>}
            ]
        )
    after
        lists:foreach(fun(M) -> meck:unload(M) end, Modules)
    end.

notification_order_test() ->
    S = fun m_mailinglist_run:next_state/2,
    ?assertEqual(<<"sent">>, S(<<"sent">>, <<"queued">>)),
    ?assertEqual(<<"sent">>, S(<<"sent">>, <<"retrying">>)),
    ?assertEqual(<<"retrying">>, S(<<"retrying">>, <<"queued">>)),
    ?assertEqual(<<"bounced">>, S(<<"sent">>, <<"bounced">>)),
    ?assertEqual(<<"bounced">>, S(<<"bounced">>, <<"sent">>)),
    ?assertEqual(<<"failed">>, S(<<"failed">>, <<"queued">>)),
    ?assertEqual(<<"cancelled">>, S(<<"cancelled">>, <<"submitting">>)).

completion_test() ->
    S = fun m_mailinglist_run:status/3,
    ?assertEqual(<<"sending">>, S(<<"sending">>, false, #{})),
    ?assertEqual(<<"empty">>, S(<<"sending">>, true, #{})),
    ?assertEqual(<<"empty">>, S(<<"sending">>, true, #{<<"skipped">> => 100})),
    ?assertEqual(<<"completed">>, S(<<"interrupted">>, true, #{<<"sent">> => 1})),
    ?assertEqual(<<"interrupted">>, S(<<"interrupted">>, true, #{<<"submitting">> => 1})),
    ?assertEqual(<<"sending">>, S(<<"sending">>, true, #{<<"queued">> => 1})),
    ?assertEqual(<<"retrying">>, S(<<"sending">>, true, #{<<"retrying">> => 1})),
    ?assertEqual(<<"completed_errors">>, S(<<"completed">>, true, #{<<"bounced">> => 1})),
    ?assertEqual(<<"completed">>, S(<<"sending">>, true, #{<<"sent">> => 10, <<"skipped">> => 2})),
    ?assertEqual(<<"cancelled">>, S(<<"cancelled">>, true, #{<<"sent">> => 3})).

back_preserves_draft_test() ->
    Draft = [
        {language, <<"nl">>},
        {fallback_language, <<"en">>},
        {language_policy, <<"matching">>},
        {audience, <<"matching_or_unset">>},
        {send_mode, <<"failed">>}
    ],
    Args = [
        {id, 1},
        {list_id, 2},
        {options, Draft},
        {mail_when, <<"date">>},
        {mailing_date, <<"2027-01-20">>},
        {mailing_time, <<"10:30">>}
    ],
    Modules = [mod_mailinglist, m_rsc, z_render],
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(mod_mailinglist, is_allowed_to_send, fun(_, _, _) -> true end),
        meck:expect(m_rsc, rid, fun(mailinglist_test, _) -> 3 end),
        meck:expect(z_render, dialog, fun(_, _, _, Context) -> Context end),
        #context{} = action_mailinglist_dialog_mailing_page:event(
            #postback{message = {mailing_back, Args}}, #context{language = [en]}
        ),
        Vars = meck:capture(1, z_render, dialog, ['_', "_dialog_mailing_page.tpl", '_', '_'], 3),
        lists:foreach(
            fun({Key, Value}) -> ?assertEqual(Value, proplists:get_value(Key, Vars)) end, Args
        )
    after
        lists:foreach(fun(M) -> catch meck:unload(M) end, Modules)
    end.

language_test() ->
    {ok, _} = application:ensure_all_started(jobs),
    %% CI already starts this queue through zotonic_core_sup. Only create it
    %% when running these tests in a standalone Erlang VM.
    case jobs:queue_info(zotonic_singular_job) of
        undefined ->
            jobs:add_queue(zotonic_singular_job, [{regulators, [{counter, [{limit, 1}]}]}]);
        {queue, _} ->
            ok
    end,
    L = fun m_mailinglist_run:language/4,
    ?assertEqual({ok, <<"nl">>}, L(<<>>, nl, <<"en">>, [en, nl])),
    ?assertEqual({ok, <<"nl">>}, L(<<>>, <<"nl-be">>, <<"en">>, [en, nl])),
    ?assertEqual({ok, <<"en">>}, L(<<>>, undefined, <<"en">>, [en, nl])),
    ?assertEqual({ok, <<"nl">>}, L(<<"nl">>, en, <<"en">>, [en, nl])),
    ?assertMatch({skip, _}, L(<<>>, fr, <<"en">>, [en, nl])),
    ?assertMatch({skip, _}, L(<<"fr">>, nl, <<"en">>, [en, nl])).

%% Opt-in integration suite. Uses only a transaction-local schema in the named
%% database, rolled back even on failure. No emails or existing site data touched.
postgres_test_() ->
    case os:getenv("MAILINGLIST_TEST_DB") of
        false -> [];
        Db -> {timeout, 60, fun() -> postgres(Db) end}
    end.

equery(C, Sql, Args, _) -> epgsql:equery(C, Sql, Args).
squery(C, Sql, _) -> epgsql:squery(C, Sql).

postgres(Db) ->
    {ok, C} = epgsql:connect(#{
        host => "localhost",
        username => os:getenv("USER"),
        database => Db,
        codecs => [{z_db_pgsql_codec, []}],
        nulls => [undefined, null]
    }),
    Context = #context{
        site = mailinglist_run_test,
        db = {mailinglist_run_test, ?MODULE},
        dbc = C,
        language = [en],
        user_id = 1,
        acl = admin
    },
    Modules = [
        z_context,
        z_stats,
        z_db,
        z_mqtt,
        m_rsc,
        m_config,
        mod_mailinglist,
        m_email_status,
        z_mailinglist_recipients,
        z_email_server,
        m_edge,
        m_mailinglist,
        z_template
    ],
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(z_context, logger_md, fun(_) -> ok end),
        meck:expect(z_context, ensure_logger_md, fun(_) -> ok end),
        meck:expect(z_context, depickle, fun(_) -> Context end),
        meck:expect(z_stats, record_event, fun(_, _, _) -> ok end),
        meck:expect(z_stats, record_duration, fun(_, _, _, _) -> ok end),
        meck:expect(z_mqtt, publish, fun(_, _, _) -> ok end),
        meck:expect(z_db, flush, fun(_) -> ok end),
        meck:expect(z_db, table_exists, fun(Table, Ctx) ->
            z_db:q1("select to_regclass($1) is not null", [atom_to_binary(Table)], Ctx)
        end),
        meck:expect(z_db, column_exists, fun(Table, Col, Ctx) ->
            z_db:q1(
                "select exists(select 1 from information_schema.columns
                where table_schema=current_schema() and table_name=$1 and column_name=$2)",
                [atom_to_binary(Table), atom_to_binary(Col)],
                Ctx
            )
        end),
        meck:expect(m_rsc, rid, fun
            (mailinglist_test, _) -> 3;
            (I, _) -> I
        end),
        meck:expect(m_rsc, p, fun
            (_, language, _) -> [en, nl];
            (_, _, _) -> undefined
        end),
        meck:expect(m_rsc, p_no_acl, fun(_, pref_language, _) -> en end),
        meck:expect(m_config, get_boolean, fun(_, _, _, _) -> false end),
        meck:expect(mod_mailinglist, is_allowed_to_send, fun(_, _, _) -> true end),
        meck:expect(mod_mailinglist, ensure_scheduled_task, fun(_) -> ok end),
        meck:expect(m_email_status, is_ok_to_send, fun(_, _) -> true end),
        meck:expect(m_mailinglist, get_email_from, fun(_, _) -> <<"test@example.com">> end),
        meck:expect(m_edge, objects, fun(_, _, _) -> [] end),
        meck:expect(z_template, render_to_iolist, fun(_, Vars, _) ->
            ?assertEqual(undefined, proplists:get_value(email, Vars)),
            ?assertEqual(undefined, proplists:get_value(recipient_key, Vars)),
            {
                [
                    <<"<html><title>Original</title><body>">>,
                    proplists:get_value(email_language, Vars),
                    <<"</body></html>">>
                ],
                undefined
            }
        end),
        meck:expect(z_mailinglist_recipients, recipient_key_encode, fun(_, _, _) ->
            {ok, <<"test">>}
        end),
        meck:expect(z_mailinglist_recipients, list_candidates, fun(_, _) ->
            #{
                <<"one@example.com">> => 1,
                <<"two@example.com">> => #{<<"pref_language">> => nl}
            }
        end),
        {ok, [], []} = epgsql:squery(C, "begin"),
        Schema = "mailinglist_test_" ++ integer_to_list(erlang:unique_integer([positive])),
        z_db:q("create schema " ++ Schema, Context),
        z_db:q("set local search_path to " ++ Schema, Context),
        z_db:q(
            "create table rsc(id integer primary key,is_published boolean default true,
            publication_start timestamptz default now(), publication_end timestamptz default '9999-01-01')",
            Context
        ),
        z_db:q("insert into rsc(id) values (1),(2),(3)", Context),
        _ = z_mailinglist_schema:manage_schema(install, Context),
        schema_migration(Context),
        language_policies(Context),
        recovery_regressions(Context),
        history_pagination(Context),
        delivery(Context),
        worker(Context),
        lifecycle(Context),
        editorial_outcomes(Context),
        recent_mailings(Context),
        access(Context),
        retention(Context)
    after
        epgsql:squery(C, "rollback"),
        epgsql:close(C),
        lists:foreach(fun(M) -> catch meck:unload(M) end, Modules)
    end.

schema_migration(Context) ->
    z_db:q(
        "insert into mailinglist_scheduled(page_id,mailinglist_id,props) values (1,2,$1)",
        [
            ?DB_PROPS([
                {options, [{is_send_all, true}]}, {pickled_context, z_context:pickle(Context)}
            ])
        ],
        Context
    ),
    _ = z_mailinglist_schema:manage_schema({upgrade, 5}, Context),
    ?assertEqual(0, z_db:q1("select count(*) from mailinglist_scheduled", Context)),
    ?assertEqual(1, z_db:q1("select count(*) from mailinglist_run", Context)),
    ?assertEqual(<<"all">>, z_db:q1("select send_mode from mailinglist_run", Context)),
    _ = z_mailinglist_schema:manage_schema({upgrade, 5}, Context),
    ?assertEqual(1, z_db:q1("select count(*) from mailinglist_run", Context)),
    z_db:q(
        "alter table mailinglist_run drop column details_expired, drop column first_submitted",
        Context
    ),
    _ = z_mailinglist_schema:manage_schema({upgrade, 6}, Context),
    ?assertEqual(
        [{undefined, undefined}],
        z_db:q("select details_expired,first_submitted from mailinglist_run", Context)
    ),
    z_db:q("delete from mailinglist_run", Context).

new_run(Options, Ctx) ->
    %% PostgreSQL now() is fixed at the start of this suite's outer transaction.
    %% Use the same clock so runs stay due even when the suite crosses a second.
    Due = z_db:q1("select now()", Ctx),
    {ok, Id} = m_mailinglist_run:create(2, 1, <<"date">>, Due, Options, Ctx),
    Id.

delivery(Ctx) ->
    Id = new_run([{request_key, <<"once">>}], Ctx),
    ?assertEqual(Id, new_run([{request_key, <<"once">>}], Ctx)),
    {ok, #{<<"id">> := Id}} = m_mailinglist_run:claim(Ctx),
    ?assertEqual({error, enoent}, m_mailinglist_run:claim(Ctx)),
    ok = m_mailinglist_run:add_recipient(
        Id, <<"a@example.com">>, undefined, <<"en">>, <<"pending">>, undefined, Ctx
    ),
    ok = m_mailinglist_run:add_recipient(
        Id, <<"a@example.com">>, undefined, <<"en">>, <<"pending">>, undefined, Ctx
    ),
    Rid = z_db:q1("select id from mailinglist_run_recipient where run_id=$1", [Id], Ctx),
    z_db:q(
        "insert into mailinglist_run_message(message_nr,recipient_id) values ('test-message',$1)",
        [Rid],
        Ctx
    ),
    z_db:q(
        "update mailinglist_run_message set created='2026-01-01 10:00:00+00' where message_nr='test-message'",
        Ctx
    ),
    {ok, {Timeline, []}} = m_mailinglist_run:m_get([<<"run">>, Id], undefined, Ctx),
    ?assertEqual({{2026, 1, 1}, {10, 0, 0}}, maps:get(<<"first_submitted">>, Timeline)),
    m_mailinglist_run:prepared(Id, Ctx),
    m_mailinglist_run:message(<<"test-message">>, <<"retrying">>, false, 2, <<"Try again">>, Ctx),
    m_mailinglist_run:message(<<"test-message">>, <<"sent">>, false, 0, undefined, Ctx),
    m_mailinglist_run:message(<<"test-message">>, <<"sent">>, true, 0, undefined, Ctx),
    m_mailinglist_run:message(<<"test-message">>, <<"queued">>, false, 0, undefined, Ctx),
    Stats = m_mailinglist_run:stats(Id, Ctx),
    ?assertEqual(1, maps:get(<<"sent">>, Stats)),
    ?assertEqual(1, maps:get(<<"total">>, Stats)),
    ?assertEqual(
        <<"completed">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    m_mailinglist_run:message(<<"test-message">>, <<"bounced">>, true, 0, undefined, Ctx),
    m_mailinglist_run:message(<<"test-message">>, <<"sent">>, true, 0, undefined, Ctx),
    ?assertEqual(0, maps:get(<<"sent">>, m_mailinglist_run:stats(Id, Ctx))),
    ?assertEqual(1, maps:get(<<"bounced">>, m_mailinglist_run:stats(Id, Ctx))),
    ?assertEqual(
        <<"completed_errors">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    ?assertEqual(
        ok, m_mailinglist_run:message(<<"unrelated">>, <<"sent">>, true, 0, undefined, Ctx)
    ).

worker(Ctx) ->
    %% Notification can arrive before the submission call returns.
    meck:expect(z_email_server, send_queued, fun(Msg, _Email, C) ->
        m_mailinglist_run:message(Msg, <<"sent">>, false, 0, undefined, C),
        {ok, Msg}
    end),
    Id = new_run([{language, <<"nl">>}, {send_mode, <<"all">>}], Ctx),
    {ok, _} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(Id, Ctx),
    {ok, NlCopy} = m_mailinglist_run:content(Id, <<"nl">>, Ctx),
    ?assertEqual(
        <<"<html><title>Original</title><body>nl</body></html>">>, maps:get(<<"html">>, NlCopy)
    ),
    ?assertEqual(
        1, z_db:q1("select count(*) from mailinglist_run_content where run_id=$1", [Id], Ctx)
    ),
    S = m_mailinglist_run:stats(Id, Ctx),
    ?assertEqual(1, maps:get(<<"sent">>, S)),
    ?assertEqual(1, maps:get(<<"skipped">>, S)),
    ?assertEqual(100, maps:get(<<"percent">>, S)),
    ?assertEqual(
        <<"completed">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    ?assert(meck:validate(z_email_server)),
    NewNl = new_run([{language, <<"nl">>}], Ctx),
    {ok, _} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(NewNl, Ctx),
    ?assertEqual(2, maps:get(<<"skipped">>, m_mailinglist_run:stats(NewNl, Ctx))),
    NewEn = new_run([{language, <<"en">>}], Ctx),
    {ok, _} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(NewEn, Ctx),
    ?assertEqual(1, maps:get(<<"sent">>, m_mailinglist_run:stats(NewEn, Ctx))),
    Cancel = new_run([{send_mode, <<"all">>}], Ctx),
    {ok, _} = m_mailinglist_run:claim(Ctx),
    meck:expect(z_email_server, send_queued, fun(Msg, _Email, C) ->
        m_mailinglist_run:cancel(Cancel, C),
        m_mailinglist_run:message(Msg, <<"sent">>, false, 0, undefined, C),
        {ok, Msg}
    end),
    ok = z_mailinglist_run:send(Cancel, Ctx),
    ?assertEqual(1, maps:get(<<"sent">>, m_mailinglist_run:stats(Cancel, Ctx))),
    ?assertEqual(1, maps:get(<<"cancelled">>, m_mailinglist_run:stats(Cancel, Ctx))),
    ?assertEqual(
        <<"cancelled">>, z_db:q1("select status from mailinglist_run where id=$1", [Cancel], Ctx)
    ).

lifecycle(Ctx) ->
    Nl = new_run([{language, <<"nl">>}], Ctx),
    En = new_run([{language, <<"en">>}], Ctx),
    ?assertNotEqual(Nl, En),
    ?assertMatch({{_, _, _}, {_, _, _}}, m_mailinglist_run:next_due(Ctx)),
    ok = m_mailinglist_run:add_recipient(
        Nl, <<"waiting@example.com">>, undefined, <<"nl">>, <<"pending">>, undefined, Ctx
    ),
    ok = m_mailinglist_run:cancel(Nl, Ctx),
    ?assertEqual(1, maps:get(<<"cancelled">>, m_mailinglist_run:stats(Nl, Ctx))),
    z_db:q(
        "update mailinglist_run set status='preparing', modified=now()-interval '11 minutes' where id=$1",
        [En],
        Ctx
    ),
    ok = m_mailinglist_run:recover(Ctx),
    ?assertEqual(
        <<"interrupted">>, z_db:q1("select status from mailinglist_run where id=$1", [En], Ctx)
    ),
    ok = m_mailinglist_run:resume(En, Ctx),
    ?assertEqual(
        <<"scheduled">>, z_db:q1("select status from mailinglist_run where id=$1", [En], Ctx)
    ),
    {ok, #{<<"id">> := En}} = m_mailinglist_run:claim(Ctx),
    ok = m_mailinglist_run:release(En, Ctx),
    ok = m_mailinglist_run:rebuild_stats(Nl, Ctx),
    ?assertEqual(1, maps:get(<<"total">>, m_mailinglist_run:stats(Nl, Ctx))),
    {ok, {Rows, []}} = m_mailinglist_run:m_get(
        [], #{payload => #{<<"status">> => <<"cancelled">>, <<"language">> => <<"nl">>}}, Ctx
    ),
    ?assert(lists:any(fun(R) -> maps:get(<<"id">>, R) =:= Nl end, Rows)),
    {ok, Child} = m_mailinglist_run:resend(Nl, <<"failed">>, Ctx),
    ?assertEqual(Nl, z_db:q1("select parent_id from mailinglist_run where id=$1", [Child], Ctx)),
    ?assertEqual(
        {error, invalid_options},
        m_mailinglist_run:create(
            2, 1, <<"date">>, calendar:universal_time(), [{language, <<"fr">>}], Ctx
        )
    ).

editorial_outcomes(Ctx) ->
    %% Resending a single-address test must never expand to the test list.
    meck:expect(z_email_server, send_queued, fun(Msg, _Mail, C) ->
        m_mailinglist_run:message(Msg, <<"sent">>, false, 0, undefined, C),
        {ok, Msg}
    end),
    {ok, Test} = m_mailinglist_run:create(
        3,
        1,
        <<"date">>,
        calendar:universal_time(),
        [{single_test_address, <<"editor@example.com">>}, {send_mode, <<"all">>}],
        Ctx
    ),
    ?assertEqual(
        #{{<<"en">>, <<"pending">>} => 1},
        z_mailinglist_run:preview(
            3,
            1,
            [{single_test_address, <<"editor@example.com">>}, {send_mode, <<"all">>}],
            Ctx
        )
    ),
    claim_test_run(Test, Ctx),
    ok = z_mailinglist_run:send(Test, Ctx),
    {ok, Again} = m_mailinglist_run:resend(Test, <<"all">>, Ctx),
    claim_test_run(Again, Ctx),
    ok = z_mailinglist_run:send(Again, Ctx),
    ?assertEqual(
        [{<<"editor@example.com">>}],
        z_db:q(
            "select email from mailinglist_run_recipient where run_id=$1", [Again], Ctx
        )
    ),
    {ok, Retry} = m_mailinglist_run:resend(Test, <<"failed">>, Ctx),
    claim_test_run(Retry, Ctx),
    ok = z_mailinglist_run:send(Retry, Ctx),
    ?assertEqual(
        <<"empty">>, z_db:q1("select status from mailinglist_run where id=$1", [Retry], Ctx)
    ),
    ?assertEqual(0, maps:get(<<"selected">>, m_mailinglist_run:stats(Retry, Ctx))),
    %% Cancellation cannot label an entirely queued mailing as stopped.
    Queued = new_run([], Ctx),
    ok = m_mailinglist_run:add_recipient(
        Queued, <<"queued@example.com">>, undefined, <<"en">>, <<"queued">>, undefined, Ctx
    ),
    claim_test_run(Queued, Ctx),
    ok = m_mailinglist_run:prepared(Queued, Ctx),
    ok = m_mailinglist_run:cancel(Queued, Ctx),
    ?assertEqual(
        <<"sending">>, z_db:q1("select status from mailinglist_run where id=$1", [Queued], Ctx)
    ),
    %% Uncertain delivery without pending work cannot be resumed by an editor.
    Uncertain = new_run([], Ctx),
    ok = m_mailinglist_run:add_recipient(
        Uncertain,
        <<"uncertain@example.com">>,
        undefined,
        <<"en">>,
        <<"submitting">>,
        undefined,
        Ctx
    ),
    z_db:q(
        "update mailinglist_run set status='interrupted',prepared=true where id=$1",
        [Uncertain],
        Ctx
    ),
    ?assertEqual({error, eacces}, m_mailinglist_run:resume(Uncertain, Ctx)).

recent_mailings(Ctx) ->
    Ids = [new_run([], Ctx) || _ <- lists:seq(1, 6)],
    %% Older active runs must not displace newer mailings on the dashboard.
    z_db:q("update mailinglist_run set status='sending' where id=$1", [hd(Ids)], Ctx),
    {ok, {Recent, []}} = m_mailinglist_run:m_get([<<"recent">>], undefined, Ctx),
    ?assertEqual(lists:sublist(lists:reverse(Ids), 5), [maps:get(<<"id">>, R) || R <- Recent]).

access(Ctx) ->
    Id = new_run([], Ctx),
    {ok, {Public, []}} = m_mailinglist_run:m_get([<<"run">>, Id], undefined, Ctx),
    ?assertNot(maps:is_key(<<"pickled_context">>, Public)),
    ?assertNot(maps:is_key(<<"options">>, Public)),
    ?assertEqual(
        {error, eacces},
        m_mailinglist_run:cancel(Id, Ctx#context{acl = undefined, acl_is_read_only = true})
    ),
    %% Denied reads return no run data, even with a valid known run number.
    meck:new(z_acl, [passthrough, no_link]),
    try
        meck:expect(z_acl, is_allowed, fun(_, _, _) -> false end),
        ?assertEqual({error, eacces}, m_mailinglist_run:m_get([<<"run">>, Id], undefined, Ctx)),
        ?assertEqual(
            {error, eacces}, m_mailinglist_run:m_get([<<"recipients">>, Id], undefined, Ctx)
        ),
        ?assertEqual({error, eacces}, m_mailinglist_run:content(Id, <<"en">>, Ctx)),
        ?assertEqual([], m_mailinglist_run:list(all, Ctx))
    after
        meck:unload(z_acl)
    end.

retention(Ctx) ->
    %% Expire both copied test addresses and per-recipient/message diagnostics.
    {ok, Old} = m_mailinglist_run:create(
        3,
        1,
        <<"date">>,
        calendar:universal_time(),
        [{single_test_address, <<"private@example.com">>}, {send_mode, <<"all">>}],
        Ctx
    ),
    ok = m_mailinglist_run:add_recipient(
        Old, <<"private@example.com">>, undefined, <<"en">>, <<"sent">>, undefined, Ctx
    ),
    Rid = z_db:q1("select id from mailinglist_run_recipient where run_id=$1", [Old], Ctx),
    z_db:q(
        "insert into mailinglist_run_message(message_nr,recipient_id,detail) values ('expired-message',$1,'private details')",
        [Rid],
        Ctx
    ),
    claim_test_run(Old, Ctx),
    ok = m_mailinglist_run:prepared(Old, Ctx),
    z_db:q(
        "insert into mailinglist_run_content(run_id,language,html) values ($1,'en','<html>Archived content</html>')",
        [Old],
        Ctx
    ),
    Before = m_mailinglist_run:stats(Old, Ctx),
    z_db:q(
        "update mailinglist_run set created=now()-interval '4 months',finished=now()-interval '4 months',error='private details' where id=$1",
        [Old],
        Ctx
    ),
    %% Keep an old but still scheduled test: its address is needed for delivery.
    {ok, Scheduled} = m_mailinglist_run:create(
        3,
        1,
        <<"date">>,
        {{2099, 1, 1}, {0, 0, 0}},
        [{single_test_address, <<"future@example.com">>}, {send_mode, <<"all">>}],
        Ctx
    ),
    z_db:q(
        "update mailinglist_run set created=now()-interval '1 year',modified=now()-interval '1 year' where id=$1",
        [Scheduled],
        Ctx
    ),
    Bulk = new_run([{send_mode, <<"all">>}], Ctx),
    z_db:q(
        "insert into mailinglist_run_recipient(run_id,email,language,status)
        select $1, 'person-' || n || '@example.com', 'en', 'sent' from generate_series(1,10001) n",
        [Bulk],
        Ctx
    ),
    ok = m_mailinglist_run:rebuild_stats(Bulk, Ctx),
    z_db:q(
        "update mailinglist_run set status='completed',prepared=true,finished=now()-interval '4 months' where id=$1",
        [Bulk],
        Ctx
    ),
    ok = m_mailinglist_run:periodic_cleanup(Ctx),
    ?assertEqual(
        2,
        z_db:q1(
            "select count(*) from mailinglist_run_recipient where run_id=any($1::bigint[])",
            [[Old, Bulk]],
            Ctx
        )
    ),
    ?assertEqual(
        0,
        z_db:q1(
            "select count(*) from mailinglist_run_message where message_nr='expired-message'", Ctx
        )
    ),
    ?assertEqual(
        [{undefined, undefined, undefined}],
        z_db:q("select props,error,request_key from mailinglist_run where id=$1", [Old], Ctx)
    ),
    ?assertEqual(Before, m_mailinglist_run:stats(Old, Ctx)),
    {ok, SavedCopy} = m_mailinglist_run:content(Old, <<"en">>, Ctx),
    ?assertEqual(<<"<html>Archived content</html>">>, maps:get(<<"html">>, SavedCopy)),
    ok = m_mailinglist_run:rebuild_stats(Old, Ctx),
    ?assertEqual(Before, m_mailinglist_run:stats(Old, Ctx)),
    ?assertEqual({ok, {[], []}}, m_mailinglist_run:m_get([<<"recipients">>, Bulk], undefined, Ctx)),
    ?assertEqual({error, history_expired}, m_mailinglist_run:resend(Old, <<"all">>, Ctx)),
    ?assertEqual(
        {error, history_expired},
        m_mailinglist_run:check_history(2, 1, [{send_mode, <<"new">>}], Ctx)
    ),
    ?assertEqual(
        {error, history_expired},
        m_mailinglist_run:check_history(2, 1, [{send_mode, <<"failed">>}], Ctx)
    ),
    ?assertEqual(ok, m_mailinglist_run:check_history(2, 1, [{send_mode, <<"all">>}], Ctx)),
    {ok, Future} = m_mailinglist_run:get(Scheduled, Ctx),
    ?assertEqual(undefined, maps:get(<<"details_expired">>, Future)),
    ?assertEqual(
        <<"future@example.com">>,
        proplists:get_value(single_test_address, maps:get(<<"options">>, Future))
    ),
    ok = m_mailinglist_run:message(
        <<"expired-message">>, <<"bounced">>, true, 0, <<"private details">>, Ctx
    ),
    ?assertEqual(Before, m_mailinglist_run:stats(Old, Ctx)),
    ok = m_mailinglist_run:periodic_cleanup(Ctx),
    ?assertEqual(
        0,
        z_db:q1(
            "select count(*) from mailinglist_run_recipient where run_id=any($1::bigint[])",
            [[Old, Bulk]],
            Ctx
        )
    ),
    ?assertEqual(10001, maps:get(<<"sent">>, m_mailinglist_run:stats(Bulk, Ctx))).

queue_ack_test_() ->
    case os:getenv("MAILINGLIST_TEST_DB") of
        false -> [];
        _ -> {timeout, 20, fun queue_ack/0}
    end.

queue_ack() ->
    %% This opt-in standalone suite must not run in a live Zotonic node.
    ?assertEqual(undefined, whereis(z_email_server)),
    Dir = filename:join(
        "/tmp", "mailing_queue_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    application:load(mnesia),
    application:set_env(mnesia, dir, Dir),
    ok = mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    try
        {atomic, ok} = mnesia:create_table(email_queue, [
            {disc_copies, [node()]},
            {attributes, [id, retry_on, retry, recipient, email, created, sent, pickled_context]}
        ]),
        ok = mnesia:wait_for_tables([email_queue], 5000),
        Email = #email{to = <<"nobody@example.com">>, queue = true},
        ?assertEqual(
            {reply, {ok, <<"queue-test">>}, test_state},
            z_email_server:handle_call(
                {enqueue, <<"queue-test">>, Email, undefined}, self(), test_state
            )
        ),
        [Original] = mnesia:dirty_read(email_queue, <<"queue-test">>),
        Sent = setelement(8, Original, os:timestamp()),
        mnesia:dirty_write(Sent),
        ?assertEqual(
            {reply, {ok, <<"queue-test">>}, test_state},
            z_email_server:handle_call(
                {enqueue, <<"queue-test">>, Email, undefined}, self(), test_state
            )
        ),
        ?assertEqual([Sent], mnesia:dirty_read(email_queue, <<"queue-test">>))
    after
        application:stop(mnesia),
        mnesia:delete_schema([node()]),
        file:del_dir_r(Dir)
    end.


%% Fixtures which target a specific run must model the scheduler's claim before
%% invoking worker APIs; other fixtures can still have scheduled work pending.
claim_test_run(Id, Ctx) ->
    1 = z_db:q(
        "update mailinglist_run set status='preparing',started=now() where id=$1 and status='scheduled'",
        [Id],
        Ctx
    ),
    ok.

recovery_regressions(Ctx) ->
    Id = new_run([{send_mode, <<"all">>}], Ctx),
    {ok, #{<<"id">> := Id}} = m_mailinglist_run:claim(Ctx),
    ok = m_mailinglist_run:add_recipient(
        Id, <<"pending@example.com">>, undefined, <<"en">>, <<"pending">>, undefined, Ctx
    ),
    ok = m_mailinglist_run:prepared(Id, Ctx),
    %% A recent worker must not be interrupted.
    ok = m_mailinglist_run:recover(Ctx),
    ?assertEqual(
        <<"sending">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    %% Simulate a worker lost after preparation, between submissions.
    z_db:q(
        "update mailinglist_run set modified=now()-interval '20 minutes' where id=$1", [Id], Ctx
    ),
    ok = m_mailinglist_run:recover(Ctx),
    ?assertEqual(
        <<"interrupted">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    ok = m_mailinglist_run:add_recipient(
        Id, <<"queued@example.com">>, undefined, <<"en">>, <<"queued">>, undefined, Ctx
    ),
    Rid = z_db:q1(
        "select id from mailinglist_run_recipient where run_id=$1 and status='queued'", [Id], Ctx
    ),
    z_db:q(
        "insert into mailinglist_run_message(message_nr,recipient_id) values ('resume-queued',$1)",
        [Rid],
        Ctx
    ),
    ok = m_mailinglist_run:resume(Id, Ctx),
    %% An old delivery result must leave the resumed run claimable.
    ok = m_mailinglist_run:message(<<"resume-queued">>, <<"sent">>, false, 0, undefined, Ctx),
    ?assertEqual(
        <<"scheduled">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    {ok, #{<<"id">> := Id}} = m_mailinglist_run:claim(Ctx),
    meck:expect(z_email_server, send_queued, fun(Msg, Mail, C) ->
        ?assertEqual(<<"pending@example.com">>, Mail#email.to),
        m_mailinglist_run:message(Msg, <<"sent">>, false, 0, undefined, C),
        {ok, Msg}
    end),
    ok = z_mailinglist_run:send(Id, Ctx),
    ?assertEqual(2, maps:get(<<"sent">>, m_mailinglist_run:stats(Id, Ctx))),
    ?assertEqual(
        <<"completed">>, z_db:q1("select status from mailinglist_run where id=$1", [Id], Ctx)
    ),
    z_db:q("delete from mailinglist_run where id=$1", [Id], Ctx),
    %% A prepared run awaiting SMTP alone must remain in progress.
    Waiting = new_run([], Ctx),
    claim_test_run(Waiting, Ctx),
    ok = m_mailinglist_run:add_recipient(
        Waiting, <<"waiting@example.com">>, undefined, <<"en">>, <<"queued">>, undefined, Ctx
    ),
    ok = m_mailinglist_run:prepared(Waiting, Ctx),
    z_db:q(
        "update mailinglist_run set modified=now()-interval '20 minutes' where id=$1",
        [Waiting],
        Ctx
    ),
    ok = m_mailinglist_run:recover(Ctx),
    ?assertEqual(
        <<"sending">>, z_db:q1("select status from mailinglist_run where id=$1", [Waiting], Ctx)
    ),
    z_db:q("delete from mailinglist_run where id=$1", [Waiting], Ctx).

history_pagination(Ctx) ->
    %% One hidden run must not remove the link to the remaining accessible run.
    z_db:q(
        "insert into mailinglist_run(page_id,mailinglist_id,status,created)\n"
        "        select case when n=201 then 3 else 1 end,2,'completed',now()+n*interval '1 second'\n"
        "        from generate_series(1,201) n",
        Ctx
    ),
    meck:new(z_acl, [passthrough, no_link]),
    try
        meck:expect(z_acl, rsc_visible, fun
            (3, _) -> false;
            (_, _) -> true
        end),
        {ok, {First, []}} = m_mailinglist_run:m_get([<<"history">>], undefined, Ctx),
        ?assertEqual(199, length(maps:get(<<"runs">>, First))),
        ?assertEqual(200, maps:get(<<"next_offset">>, First)),
        ?assertEqual(maps:get(<<"runs">>, First), m_mailinglist_run:list(all, Ctx)),
        {ok, {Second, []}} = m_mailinglist_run:m_get(
            [<<"history">>], #{payload => #{<<"offset">> => 200}}, Ctx
        ),
        ?assertEqual(1, length(maps:get(<<"runs">>, Second))),
        ?assertEqual(undefined, maps:get(<<"next_offset">>, Second)),
        %% An entirely hidden page must still allow navigation through history.
        z_db:q(
            "update mailinglist_run set page_id=3 where id in (select id from mailinglist_run order by created desc limit 200)",
            Ctx
        ),
        {ok, {Hidden, []}} = m_mailinglist_run:m_get([<<"history">>], undefined, Ctx),
        ?assertEqual([], maps:get(<<"runs">>, Hidden)),
        ?assertEqual(200, maps:get(<<"next_offset">>, Hidden)),
        meck:expect(z_acl, is_allowed, fun(_, _, _) -> false end),
        ?assertEqual(
            {ok, {#{<<"runs">> => [], <<"next_offset">> => undefined}, []}},
            m_mailinglist_run:m_get([<<"history">>], undefined, Ctx)
        )
    after
        meck:unload(z_acl),
        z_db:q("delete from mailinglist_run", Ctx)
    end.


language_policies(Ctx) ->
    Candidates = #{
        <<"resource@example.com">> => 1,
        <<"english@example.com">> => #{<<"pref_language">> => en},
        <<"dutch@example.com">> => #{<<"pref_language">> => <<"nl-be">>},
        <<"french@example.com">> => #{<<"pref_language">> => fr},
        <<"unknown@example.com">> => #{<<"pref_language">> => <<"not-a-language">>},
        <<"unset@example.com">> => #{}
    },
    Base = [{fallback_language, <<"en">>}, {send_mode, <<"all">>}],
    meck:expect(z_mailinglist_recipients, list_candidates, fun(_, _) -> Candidates end),
    try
        All = [{language_policy, <<"all">>} | Base],
        Matching = [{language_policy, <<"matching">>} | Base],
        EnglishOnly = [{language, <<"en">>}, {audience, <<"matching">>} | Base],
        EnglishWithUnset = [{language, <<"en">>}, {audience, <<"matching_or_unset">>} | Base],
        DutchWithUnset = [{language, <<"nl">>}, {audience, <<"matching_or_unset">>} | Base],
        lists:foreach(
            fun({Options, Language, Count}) ->
                ?assertEqual(
                    #{{Language, <<"pending">>} => Count, {Language, <<"skipped">>} => 6 - Count},
                    z_mailinglist_run:preview(2, 1, Options, Ctx)
                )
            end,
            [{EnglishOnly, <<"en">>, 2}, {EnglishWithUnset, <<"en">>, 3},
             {DutchWithUnset, <<"nl">>, 2}]
        ),
        ?assertEqual(
            #{{<<"en">>, <<"pending">>} => 5, {<<"nl">>, <<"pending">>} => 1},
            z_mailinglist_run:preview(2, 1, All, Ctx)
        ),
        Review = z_mailinglist_run:review(2, 1, Matching, Ctx),
        ?assertEqual(
            #{
                {<<"en">>, <<"pending">>} => 2,
                {<<"nl">>, <<"pending">>} => 1,
                {<<"en">>, <<"skipped">>} => 3
            },
            maps:get(counts, Review)
        ),
        ?assertEqual(
            #{
                <<"Missing translation">> => 1,
                <<"Unknown recipient language">> => 1,
                <<"No preferred language">> => 1
            },
            maps:get(reasons, Review)
        ),
        %% Existing schedules without a policy retain the old language behavior.
        ?assertEqual(
            #{
                {<<"en">>, <<"pending">>} => 3,
                {<<"nl">>, <<"pending">>} => 1,
                {<<"en">>, <<"skipped">>} => 2
            },
            z_mailinglist_run:preview(2, 1, Base, Ctx)
        ),
        %% Choosing one language for everyone is independent of the automatic policy.
        ?assertEqual(
            #{{<<"en">>, <<"pending">>} => 6},
            z_mailinglist_run:preview(
                2, 1, [{language, <<"en">>}, {audience, <<"all">>} | Matching], Ctx
            )
        ),
        lists:foreach(
            fun({Options, Count}) ->
                Id = new_run(Options, Ctx),
                {ok, #{<<"id">> := Id}} = m_mailinglist_run:claim(Ctx),
                meck:expect(z_email_server, send_queued, fun(Msg, Mail, C) ->
                    ExpectedLanguage =
                        case proplists:get_value(language, Options) of
                            undefined ->
                                case Mail#email.to of
                                    <<"dutch@example.com">> -> <<"nl">>;
                                    _ -> <<"en">>
                                end;
                            SelectedLanguage -> SelectedLanguage
                        end,
                    ?assertEqual(
                        ExpectedLanguage, proplists:get_value(email_language, Mail#email.vars)
                    ),
                    m_mailinglist_run:message(Msg, <<"sent">>, false, 0, undefined, C),
                    {ok, Msg}
                end),
                ok = z_mailinglist_run:send(Id, Ctx),
                ?assertEqual(Count, maps:get(<<"sent">>, m_mailinglist_run:stats(Id, Ctx))),
                {ok, {Public, []}} = m_mailinglist_run:m_get([<<"run">>, Id], undefined, Ctx),
                ?assertEqual(
                    proplists:get_value(language_policy, Options),
                    maps:get(<<"language_policy">>, Public)
                ),
                {ok, Again} = m_mailinglist_run:resend(Id, <<"all">>, Ctx),
                {ok, Resend} = m_mailinglist_run:get(Again, Ctx),
                ?assertEqual(proplists:get_value(audience, Options, <<"matching">>),
                    maps:get(<<"audience">>, Resend)),
                ?assertEqual(
                    proplists:get_value(language_policy, Options),
                    proplists:get_value(language_policy, maps:get(<<"options">>, Resend))
                ),
                z_db:q("delete from mailinglist_run where id=any($1::bigint[])", [[Id, Again]], Ctx)
            end,
            [{All, 6}, {Matching, 3}, {EnglishOnly, 2}, {EnglishWithUnset, 3}, {DutchWithUnset, 2}]
        ),
        ?assertEqual(
            {error, invalid_options},
            m_mailinglist_run:create(
                2, 1, <<"date">>, calendar:universal_time(), [{language_policy, <<"invalid">>}], Ctx
            )
        )
    after
        meck:expect(z_mailinglist_recipients, list_candidates, fun(_, _) ->
            #{<<"one@example.com">> => 1, <<"two@example.com">> => #{<<"pref_language">> => nl}}
        end)
    end.

immediate_publication_test() ->
    Modules = [m_mailinglist_run, z_mailinglist_run, m_rsc, z_render, z_sidejob],
    Context = #context{language = [en]},
    Future = {{2099, 1, 1}, {10, 0, 0}},
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(z_sidejob, start, fun(_, _, _, _) -> {ok, self()} end),
        meck:expect(m_mailinglist_run, allowed, fun(_, _) -> true end),
        meck:expect(z_mailinglist_run, review, fun(_, _, _, _) ->
            #{counts => #{{<<"en">>, <<"pending">>} => 1}, reasons => #{}}
        end),
        meck:expect(m_rsc, rid, fun(mailinglist_test, _) -> 3 end),
        meck:expect(z_render, dialog, fun(_, _, _, C) -> C end),
        lists:foreach(
            fun({List, Start, Type}) ->
                Run = #{
                    <<"id">> => 1, <<"page_id">> => 1, <<"mailinglist_id">> => List,
                    <<"language">> => <<>>, <<"fallback_language">> => <<"en">>,
                    <<"audience">> => <<"all">>, <<"options">> => []
                },
                meck:expect(m_mailinglist_run, get, fun(_, _) -> {ok, Run} end),
                meck:expect(m_rsc, p, fun(1, publication_start, _) -> Start end),
                meck:reset(z_render),
                #context{} = action_mailinglist_dialog_mailing_page:event(
                    #postback{message = {mailing_resend_review, [{run_id, 1}, {mode, <<"all">>}]}},
                    Context
                ),
                Vars = meck:capture(1, z_render, dialog, ['_', "_dialog_mailing_count.tpl", '_', '_'], 3),
                ?assertEqual(Type, proplists:get_value(type, Vars)),
                Due = proplists:get_value(due, Vars),
                case {List, Start} of
                    {2, Future} -> ?assertEqual(Future, Due);
                    _ -> ?assert(Due =< calendar:universal_time())
                end
            end,
            [{2, Future, <<"publication">>},
             {2, {{2020, 1, 1}, {0, 0, 0}}, <<"publication">>},
             {2, undefined, <<"publication">>},
             {3, Future, <<"date">>}]
        ),
        meck:expect(z_sidejob, start, fun(_, _, _, _) -> {error, overload} end),
        #context{} = action_mailinglist_dialog_mailing_page:event(
            #postback{message = {mailing_resend_review, [{run_id, 1}, {mode, <<"all">>}]}},
            Context
        ),
        Busy = meck:capture(1, z_render, dialog, ['_', "_dialog_mailing_count_error.tpl", '_', '_'], 3),
        ?assertEqual(overload, proplists:get_value(error, Busy))
    after
        lists:foreach(fun(M) -> catch meck:unload(M) end, Modules)
    end.

%% A blocked count must not block its postback or offer a send button early.
async_review_test() ->
    Modules = [z_mailinglist_run, mod_mailinglist, m_rsc, z_render, z_transport],
    Context = #context{language = [en]},
    Parent = self(),
    Vars = [{id, 1}, {list_id, 2}, {options, []}],
    Start = fun() ->
        spawn_monitor(fun() ->
            action_mailinglist_dialog_mailing_page:await_review(Vars, Context)
        end)
    end,
    Event = fun(Pid) ->
        #postback{
            message =
                {mailing_review_count, [{target, <<"draft-1">>}, {draft, Vars}, {count_pid, Pid}]}
        }
    end,
    Back = fun(Pid) ->
        #postback{message = {mailing_back, [{count_pid, Pid} | Vars]}}
    end,
    try
        lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
        meck:expect(mod_mailinglist, is_allowed_to_send, fun(_, _, _) -> true end),
        meck:expect(m_rsc, rid, fun(mailinglist_test, _) -> 3 end),
        meck:expect(z_render, dialog, fun(_, _, _, C) -> C end),
        meck:expect(z_render, update, fun(Target, Render, C) ->
            Parent ! {updated, Target, Render},
            C
        end),
        meck:expect(z_transport, reply_actions, fun(_) -> ok end),
        meck:expect(z_mailinglist_run, review, fun(_, _, _, _) ->
            Parent ! {counting, self()},
            receive
                finish_count ->
                    #{counts => #{{<<"en">>, <<"pending">>} => 7}, reasons => #{}}
            after 2000 -> error(test_count_not_released)
            end
        end),
        {Worker, Ref} = Start(),
        ?assertEqual(Context, action_mailinglist_dialog_mailing_page:event(Event(Worker), Context)),
        receive
            {counting, Worker} -> ok
        after 1000 -> error(no_count_worker)
        end,
        ?assertEqual(0, meck:num_calls(z_render, update, '_')),
        Worker ! finish_count,
        receive
            {updated, <<"draft-1">>, #render{
                template = "_dialog_mailing_review.tpl", vars = Review
            }} ->
                ?assertEqual(7, proplists:get_value(eligible, Review))
        after 1000 -> error(no_review)
        end,
        receive
            {'DOWN', Ref, process, Worker, normal} -> ok
        after 1000 -> error(worker_not_done)
        end,
        %% Back terminates both an active count and a not-yet-started count.
        lists:foreach(
            fun(IsStarted) ->
                {Pid, Monitor} = Start(),
                case IsStarted of
                    true ->
                        action_mailinglist_dialog_mailing_page:event(Event(Pid), Context),
                        receive
                            {counting, Pid} -> ok
                        after 1000 -> error(no_count_worker)
                        end;
                    false ->
                        ok
                end,
                ?assertEqual(
                    Context, action_mailinglist_dialog_mailing_page:event(Back(Pid), Context)
                ),
                receive
                    {'DOWN', Monitor, process, Pid, shutdown} -> ok
                after 1000 -> error(count_not_cancelled)
                end
            end,
            [true, false]
        ),
        ?assertEqual(1, meck:num_calls(z_render, update, '_')),
        %% Timeout exceptions and gen_server timeout exits must all preserve
        %% the draft and provide a retry path without logging an error stack.
        lists:foreach(
            fun(Fail) ->
                meck:expect(z_mailinglist_run, review, fun(_, _, _, _) -> Fail() end),
                ?assertEqual(ok,
                    action_mailinglist_dialog_mailing_page:review_async(<<"draft-2">>, Vars, Context)),
                receive
                    {updated, <<"draft-2">>, #render{
                        template = "_dialog_mailing_count_error.tpl", vars = ErrorVars
                    }} ->
                        ?assertEqual(count_failed, proplists:get_value(error, ErrorVars)),
                        ?assertEqual([], proplists:get_value(options, ErrorVars))
                after 1000 -> error(no_count_error)
                end
            end,
            [
                fun() -> error(timeout) end,
                fun() -> exit(timeout) end,
                fun() -> exit({timeout, {gen_server, call, [self(), count]}}) end,
                fun() -> error({badmatch, {error, timeout}}) end
            ]),
        meck:expect(mod_mailinglist, is_allowed_to_send, fun(_, _, _) -> false end),
        %% Permissions can change after admission but before the worker runs.
        CallsBeforeDenied = meck:num_calls(z_mailinglist_run, review, '_'),
        ?assertEqual(ok,
            action_mailinglist_dialog_mailing_page:review_async(<<"draft-denied">>, Vars, Context)),
        ?assertEqual(CallsBeforeDenied, meck:num_calls(z_mailinglist_run, review, '_')),
        receive
            {updated, <<"draft-denied">>, #render{
                template = "_dialog_mailing_count_error.tpl", vars = WorkerDeniedVars
            }} ->
                ?assertEqual(eacces, proplists:get_value(error, WorkerDeniedVars))
        after 1000 -> error(no_worker_permission_error)
        end,
        {Denied, DeniedRef} = Start(),
        ?assertEqual(Context, action_mailinglist_dialog_mailing_page:event(Event(Denied), Context)),
        receive
            {'DOWN', DeniedRef, process, Denied, shutdown} -> ok
        after 1000 -> error(denied_worker_not_cancelled)
        end,
        receive
            {updated, <<"draft-1">>, #render{vars = DeniedVars}} ->
                ?assertEqual(eacces, proplists:get_value(error, DeniedVars))
        after 1000 -> error(no_permission_error)
        end
    after
        lists:foreach(fun(M) -> catch meck:unload(M) end, Modules)
    end.

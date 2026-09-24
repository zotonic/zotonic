%% Copyright 2026 The Zotonic Contributors
%% SPDX-License-Identifier: Apache-2.0
-module(m_mailinglist_run_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([equery/4,squery/3]).

notification_order_test() ->
    S = fun m_mailinglist_run:next_state/2,
    ?assertEqual(<<"sent">>,S(<<"sent">>,<<"queued">>)),
    ?assertEqual(<<"sent">>,S(<<"sent">>,<<"retrying">>)),
    ?assertEqual(<<"retrying">>,S(<<"retrying">>,<<"queued">>)),
    ?assertEqual(<<"bounced">>,S(<<"sent">>,<<"bounced">>)),
    ?assertEqual(<<"bounced">>,S(<<"bounced">>,<<"sent">>)),
    ?assertEqual(<<"failed">>,S(<<"failed">>,<<"queued">>)),
    ?assertEqual(<<"cancelled">>,S(<<"cancelled">>,<<"submitting">>)).

completion_test() ->
    S = fun m_mailinglist_run:status/3,
    ?assertEqual(<<"sending">>,S(<<"sending">>,false,#{})),
    ?assertEqual(<<"empty">>,S(<<"sending">>,true,#{})),
    ?assertEqual(<<"empty">>,S(<<"sending">>,true,#{<<"skipped">> => 100})),
    ?assertEqual(<<"completed">>,S(<<"interrupted">>,true,#{<<"sent">> => 1})),
    ?assertEqual(<<"interrupted">>,S(<<"interrupted">>,true,#{<<"submitting">> => 1})),
    ?assertEqual(<<"sending">>,S(<<"sending">>,true,#{<<"queued">> => 1})),
    ?assertEqual(<<"retrying">>,S(<<"sending">>,true,#{<<"retrying">> => 1})),
    ?assertEqual(<<"completed_errors">>,S(<<"completed">>,true,#{<<"bounced">> => 1})),
    ?assertEqual(<<"completed">>,S(<<"sending">>,true,#{<<"sent">> => 10,<<"skipped">> => 2})),
    ?assertEqual(<<"cancelled">>,S(<<"cancelled">>,true,#{<<"sent">> => 3})).

back_preserves_draft_test() ->
    Draft = [{language,<<"nl">>},{fallback_language,<<"en">>},
        {audience,<<"all">>},{send_mode,<<"failed">>}],
    Args = [{id,1},{list_id,2},{options,Draft},{mail_when,<<"date">>},
        {mailing_date,<<"2027-01-20">>},{mailing_time,<<"10:30">>}],
    Modules = [mod_mailinglist,m_rsc,z_render],
    try
        lists:foreach(fun(M) -> meck:new(M,[passthrough,no_link]) end,Modules),
        meck:expect(mod_mailinglist,is_allowed_to_send,fun(_,_,_) -> true end),
        meck:expect(m_rsc,rid,fun(mailinglist_test,_) -> 3 end),
        meck:expect(z_render,dialog,fun(_,Template,Vars,_) -> {Template,Vars} end),
        {"_dialog_mailing_page.tpl",Vars} = action_mailinglist_dialog_mailing_page:event(
            #postback{message={mailing_back,Args}},#context{language=[en]}),
        lists:foreach(fun({Key,Value}) -> ?assertEqual(Value,proplists:get_value(Key,Vars)) end,Args)
    after lists:foreach(fun(M) -> catch meck:unload(M) end,Modules) end.

language_test() ->
    {ok, _} = application:ensure_all_started(jobs),
    %% CI already starts this queue through zotonic_core_sup. Only create it
    %% when running these tests in a standalone Erlang VM.
    case jobs:queue_info(zotonic_singular_job) of
        undefined -> jobs:add_queue(zotonic_singular_job,[{regulators,[{counter,[{limit,1}]}]}]);
        {queue, _} -> ok
    end,
    L = fun m_mailinglist_run:language/4,
    ?assertEqual({ok,<<"nl">>},L(<<>>,nl,<<"en">>,[en,nl])),
    ?assertEqual({ok,<<"en">>},L(<<>>,undefined,<<"en">>,[en,nl])),
    ?assertEqual({ok,<<"nl">>},L(<<"nl">>,en,<<"en">>,[en,nl])),
    ?assertMatch({skip,_},L(<<>>,fr,<<"en">>,[en,nl])),
    ?assertMatch({skip,_},L(<<"fr">>,nl,<<"en">>,[en,nl])).

%% Opt-in integration suite. Uses only a transaction-local schema in the named
%% database, rolled back even on failure. No emails or existing site data touched.
postgres_test_() ->
    case os:getenv("MAILINGLIST_TEST_DB") of
        false -> [];
        Db -> {timeout,60,fun() -> postgres(Db) end}
    end.

equery(C,Sql,Args,_) -> epgsql:equery(C,Sql,Args).
squery(C,Sql,_) -> epgsql:squery(C,Sql).

postgres(Db) ->
    {ok,C} = epgsql:connect(#{host => "localhost",username => os:getenv("USER"),
        database => Db,codecs => [{z_db_pgsql_codec,[]}],nulls => [undefined,null]}),
    Context = #context{site=mailinglist_run_test,db={mailinglist_run_test,?MODULE},dbc=C,
        language=[en],user_id=1,acl=admin},
    Modules = [z_context,z_stats,z_db,z_mqtt,m_rsc,m_config,mod_mailinglist,
        m_email_status,z_mailinglist_recipients,z_email_server,m_edge,m_mailinglist],
    try
        lists:foreach(fun(M) -> meck:new(M,[passthrough,no_link]) end,Modules),
        meck:expect(z_context,logger_md,fun(_) -> ok end),
        meck:expect(z_context,ensure_logger_md,fun(_) -> ok end),
        meck:expect(z_context,depickle,fun(_) -> Context end),
        meck:expect(z_stats,record_event,fun(_,_,_) -> ok end),
        meck:expect(z_stats,record_duration,fun(_,_,_,_) -> ok end),
        meck:expect(z_mqtt,publish,fun(_,_,_) -> ok end),
        meck:expect(z_db,flush,fun(_) -> ok end),
        meck:expect(z_db,table_exists,fun(Table,Ctx) ->
            z_db:q1("select to_regclass($1) is not null",[atom_to_binary(Table)],Ctx)
        end),
        meck:expect(z_db,column_exists,fun(Table,Col,Ctx) ->
            z_db:q1("select exists(select 1 from information_schema.columns
                where table_schema=current_schema() and table_name=$1 and column_name=$2)",
                [atom_to_binary(Table),atom_to_binary(Col)],Ctx)
        end),
        meck:expect(m_rsc,rid,fun(mailinglist_test,_) -> 3; (I,_) -> I end),
        meck:expect(m_rsc,p,fun(_,language,_) -> [en,nl]; (_,_,_) -> undefined end),
        meck:expect(m_rsc,p_no_acl,fun(_,pref_language,_) -> en end),
        meck:expect(m_config,get_boolean,fun(_,_,_,_) -> false end),
        meck:expect(mod_mailinglist,is_allowed_to_send,fun(_,_,_) -> true end),
        meck:expect(mod_mailinglist,ensure_scheduled_task,fun(_) -> ok end),
        meck:expect(m_email_status,is_ok_to_send,fun(_,_) -> true end),
        meck:expect(m_mailinglist,get_email_from,fun(_,_) -> <<"test@example.com">> end),
        meck:expect(m_edge,objects,fun(_,_,_) -> [] end),
        meck:expect(z_mailinglist_recipients,recipient_key_encode,fun(_,_,_) -> {ok,<<"test">>} end),
        meck:expect(z_mailinglist_recipients,list_candidates,fun(_,_) ->
            #{<<"one@example.com">> => 1,
              <<"two@example.com">> => #{<<"pref_language">> => nl}}
        end),
        {ok,[],[]} = epgsql:squery(C,"begin"),
        Schema = "mailinglist_test_" ++ integer_to_list(erlang:unique_integer([positive])),
        z_db:q("create schema " ++ Schema,Context),
        z_db:q("set local search_path to " ++ Schema,Context),
        z_db:q("create table rsc(id integer primary key,is_published boolean default true,
            publication_start timestamptz default now(), publication_end timestamptz default '9999-01-01')",Context),
        z_db:q("insert into rsc(id) values (1),(2),(3)",Context),
        _ = z_mailinglist_schema:manage_schema(install,Context),
        schema_migration(Context),
        delivery(Context),
        worker(Context),
        lifecycle(Context),
        editorial_outcomes(Context),
        access(Context)
    after
        epgsql:squery(C,"rollback"), epgsql:close(C),
        lists:foreach(fun(M) -> catch meck:unload(M) end,Modules)
    end.

schema_migration(Context) ->
    z_db:q("insert into mailinglist_scheduled(page_id,mailinglist_id,props) values (1,2,$1)",
        [?DB_PROPS([{options,[{is_send_all,true}]},{pickled_context,z_context:pickle(Context)}])],Context),
    _ = z_mailinglist_schema:manage_schema({upgrade,5},Context),
    ?assertEqual(0,z_db:q1("select count(*) from mailinglist_scheduled",Context)),
    ?assertEqual(1,z_db:q1("select count(*) from mailinglist_run",Context)),
    ?assertEqual(<<"all">>,z_db:q1("select send_mode from mailinglist_run",Context)),
    _ = z_mailinglist_schema:manage_schema({upgrade,5},Context),
    ?assertEqual(1,z_db:q1("select count(*) from mailinglist_run",Context)),
    z_db:q("delete from mailinglist_run",Context).

new_run(Options,Ctx) ->
    {ok,Id} = m_mailinglist_run:create(2,1,<<"date">>,calendar:universal_time(),Options,Ctx),Id.

delivery(Ctx) ->
    Id = new_run([{request_key,<<"once">>}],Ctx),
    ?assertEqual(Id,new_run([{request_key,<<"once">>}],Ctx)),
    {ok,#{<<"id">> := Id}} = m_mailinglist_run:claim(Ctx),
    ?assertEqual({error,enoent},m_mailinglist_run:claim(Ctx)),
    ok = m_mailinglist_run:add_recipient(Id,<<"a@example.com">>,undefined,<<"en">>,<<"pending">>,undefined,Ctx),
    ok = m_mailinglist_run:add_recipient(Id,<<"a@example.com">>,undefined,<<"en">>,<<"pending">>,undefined,Ctx),
    Rid = z_db:q1("select id from mailinglist_run_recipient where run_id=$1",[Id],Ctx),
    z_db:q("insert into mailinglist_run_message(message_nr,recipient_id) values ('test-message',$1)",[Rid],Ctx),
    m_mailinglist_run:prepared(Id,Ctx),
    m_mailinglist_run:message(<<"test-message">>,<<"retrying">>,false,2,<<"Try again">>,Ctx),
    m_mailinglist_run:message(<<"test-message">>,<<"sent">>,false,0,undefined,Ctx),
    m_mailinglist_run:message(<<"test-message">>,<<"sent">>,true,0,undefined,Ctx),
    m_mailinglist_run:message(<<"test-message">>,<<"queued">>,false,0,undefined,Ctx),
    Stats = m_mailinglist_run:stats(Id,Ctx),
    ?assertEqual(1,maps:get(<<"sent">>,Stats)),
    ?assertEqual(1,maps:get(<<"total">>,Stats)),
    ?assertEqual(<<"completed">>,z_db:q1("select status from mailinglist_run where id=$1",[Id],Ctx)),
    m_mailinglist_run:message(<<"test-message">>,<<"bounced">>,true,0,undefined,Ctx),
    m_mailinglist_run:message(<<"test-message">>,<<"sent">>,true,0,undefined,Ctx),
    ?assertEqual(0,maps:get(<<"sent">>,m_mailinglist_run:stats(Id,Ctx))),
    ?assertEqual(1,maps:get(<<"bounced">>,m_mailinglist_run:stats(Id,Ctx))),
    ?assertEqual(<<"completed_errors">>,z_db:q1("select status from mailinglist_run where id=$1",[Id],Ctx)),
    ?assertEqual(ok,m_mailinglist_run:message(<<"unrelated">>,<<"sent">>,true,0,undefined,Ctx)).

worker(Ctx) ->
    %% Notification can arrive before the submission call returns.
    meck:expect(z_email_server,send_queued,fun(Msg,_Email,C) ->
        m_mailinglist_run:message(Msg,<<"sent">>,false,0,undefined,C), {ok,Msg}
    end),
    Id = new_run([{language,<<"nl">>},{send_mode,<<"all">>}],Ctx),
    {ok,_} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(Id,Ctx),
    S = m_mailinglist_run:stats(Id,Ctx),
    ?assertEqual(1,maps:get(<<"sent">>,S)),
    ?assertEqual(1,maps:get(<<"skipped">>,S)),
    ?assertEqual(100,maps:get(<<"percent">>,S)),
    ?assertEqual(<<"completed">>,z_db:q1("select status from mailinglist_run where id=$1",[Id],Ctx)),
    ?assert(meck:validate(z_email_server)),
    NewNl = new_run([{language,<<"nl">>}],Ctx),
    {ok,_} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(NewNl,Ctx),
    ?assertEqual(2,maps:get(<<"skipped">>,m_mailinglist_run:stats(NewNl,Ctx))),
    NewEn = new_run([{language,<<"en">>}],Ctx),
    {ok,_} = m_mailinglist_run:claim(Ctx),
    ok = z_mailinglist_run:send(NewEn,Ctx),
    ?assertEqual(1,maps:get(<<"sent">>,m_mailinglist_run:stats(NewEn,Ctx))),
    Cancel = new_run([{send_mode,<<"all">>}],Ctx),
    {ok,_} = m_mailinglist_run:claim(Ctx),
    meck:expect(z_email_server,send_queued,fun(Msg,_Email,C) ->
        m_mailinglist_run:cancel(Cancel,C),
        m_mailinglist_run:message(Msg,<<"sent">>,false,0,undefined,C), {ok,Msg}
    end),
    ok = z_mailinglist_run:send(Cancel,Ctx),
    ?assertEqual(1,maps:get(<<"sent">>,m_mailinglist_run:stats(Cancel,Ctx))),
    ?assertEqual(1,maps:get(<<"cancelled">>,m_mailinglist_run:stats(Cancel,Ctx))),
    ?assertEqual(<<"cancelled">>,z_db:q1("select status from mailinglist_run where id=$1",[Cancel],Ctx)).

lifecycle(Ctx) ->
    Nl = new_run([{language,<<"nl">>}],Ctx),
    En = new_run([{language,<<"en">>}],Ctx),
    ?assertNotEqual(Nl,En),
    ?assertMatch({{_,_,_},{_,_,_}},m_mailinglist_run:next_due(Ctx)),
    ok = m_mailinglist_run:add_recipient(Nl,<<"waiting@example.com">>,undefined,<<"nl">>,<<"pending">>,undefined,Ctx),
    ok = m_mailinglist_run:cancel(Nl,Ctx),
    ?assertEqual(1,maps:get(<<"cancelled">>,m_mailinglist_run:stats(Nl,Ctx))),
    z_db:q("update mailinglist_run set status='preparing', modified=now()-interval '11 minutes' where id=$1",[En],Ctx),
    ok = m_mailinglist_run:recover(Ctx),
    ?assertEqual(<<"interrupted">>,z_db:q1("select status from mailinglist_run where id=$1",[En],Ctx)),
    ok = m_mailinglist_run:resume(En,Ctx),
    ?assertEqual(<<"scheduled">>,z_db:q1("select status from mailinglist_run where id=$1",[En],Ctx)),
    {ok,#{<<"id">> := En}} = m_mailinglist_run:claim(Ctx),
    ok = m_mailinglist_run:release(En,Ctx),
    ok = m_mailinglist_run:rebuild_stats(Nl,Ctx),
    ?assertEqual(1,maps:get(<<"total">>,m_mailinglist_run:stats(Nl,Ctx))),
    {ok,{Rows,[]}} = m_mailinglist_run:m_get([],#{payload => #{<<"status">> => <<"cancelled">>,<<"language">> => <<"nl">>}},Ctx),
    ?assert(lists:any(fun(R) -> maps:get(<<"id">>,R) =:= Nl end,Rows)),
    {ok,Child} = m_mailinglist_run:resend(Nl,<<"failed">>,Ctx),
    ?assertEqual(Nl,z_db:q1("select parent_id from mailinglist_run where id=$1",[Child],Ctx)),
    ?assertEqual({error,invalid_options},m_mailinglist_run:create(2,1,<<"date">>,calendar:universal_time(),[{language,<<"fr">>}],Ctx)).

editorial_outcomes(Ctx) ->
    %% Resending a single-address test must never expand to the test list.
    meck:expect(z_email_server,send_queued,fun(Msg,_Mail,C) ->
        m_mailinglist_run:message(Msg,<<"sent">>,false,0,undefined,C), {ok,Msg}
    end),
    {ok,Test} = m_mailinglist_run:create(3,1,<<"date">>,calendar:universal_time(),
        [{single_test_address,<<"editor@example.com">>},{send_mode,<<"all">>}],Ctx),
    ?assertEqual(#{{<<"en">>,<<"pending">>} => 1},z_mailinglist_run:preview(3,1,
        [{single_test_address,<<"editor@example.com">>},{send_mode,<<"all">>}],Ctx)),
    ok = z_mailinglist_run:send(Test,Ctx),
    {ok,Again} = m_mailinglist_run:resend(Test,<<"all">>,Ctx),
    ok = z_mailinglist_run:send(Again,Ctx),
    ?assertEqual([{<<"editor@example.com">>}],z_db:q(
        "select email from mailinglist_run_recipient where run_id=$1",[Again],Ctx)),
    {ok,Retry} = m_mailinglist_run:resend(Test,<<"failed">>,Ctx),
    ok = z_mailinglist_run:send(Retry,Ctx),
    ?assertEqual(<<"empty">>,z_db:q1("select status from mailinglist_run where id=$1",[Retry],Ctx)),
    ?assertEqual(0,maps:get(<<"selected">>,m_mailinglist_run:stats(Retry,Ctx))),
    %% Cancellation cannot label an entirely queued mailing as stopped.
    Queued = new_run([],Ctx),
    ok = m_mailinglist_run:add_recipient(Queued,<<"queued@example.com">>,undefined,<<"en">>,<<"queued">>,undefined,Ctx),
    ok = m_mailinglist_run:prepared(Queued,Ctx),
    ok = m_mailinglist_run:cancel(Queued,Ctx),
    ?assertEqual(<<"sending">>,z_db:q1("select status from mailinglist_run where id=$1",[Queued],Ctx)),
    %% Uncertain delivery without pending work cannot be resumed by an editor.
    Uncertain = new_run([],Ctx),
    ok = m_mailinglist_run:add_recipient(Uncertain,<<"uncertain@example.com">>,undefined,<<"en">>,<<"submitting">>,undefined,Ctx),
    z_db:q("update mailinglist_run set status='interrupted',prepared=true where id=$1",[Uncertain],Ctx),
    ?assertEqual({error,eacces},m_mailinglist_run:resume(Uncertain,Ctx)).

access(Ctx) ->
    Id = new_run([],Ctx),
    {ok,{Public,[]}} = m_mailinglist_run:m_get([<<"run">>,Id],undefined,Ctx),
    ?assertNot(maps:is_key(<<"pickled_context">>,Public)),
    ?assertNot(maps:is_key(<<"options">>,Public)),
    ?assertEqual({error,eacces},m_mailinglist_run:cancel(Id,Ctx#context{acl=undefined,acl_is_read_only=true})),
    %% Denied reads return no run data, even with a valid known run number.
    meck:new(z_acl,[passthrough,no_link]),
    try
        meck:expect(z_acl,is_allowed,fun(_,_,_) -> false end),
        ?assertEqual({error,eacces},m_mailinglist_run:m_get([<<"run">>,Id],undefined,Ctx)),
        ?assertEqual({error,eacces},m_mailinglist_run:m_get([<<"recipients">>,Id],undefined,Ctx)),
        ?assertEqual([],m_mailinglist_run:list(all,Ctx))
    after meck:unload(z_acl) end.

queue_ack_test_() ->
    case os:getenv("MAILINGLIST_TEST_DB") of
        false -> [];
        _ -> {timeout,20,fun queue_ack/0}
    end.

queue_ack() ->
    %% This opt-in standalone suite must not run in a live Zotonic node.
    ?assertEqual(undefined,whereis(z_email_server)),
    Dir = filename:join("/tmp","mailing_queue_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    application:load(mnesia),
    application:set_env(mnesia,dir,Dir),
    ok = mnesia:create_schema([node()]),
    {ok,_} = application:ensure_all_started(mnesia),
    try
        {atomic,ok} = mnesia:create_table(email_queue,[{disc_copies,[node()]},
            {attributes,[id,retry_on,retry,recipient,email,created,sent,pickled_context]}]),
        ok = mnesia:wait_for_tables([email_queue],5000),
        Email = #email{to = <<"nobody@example.com">>,queue=true},
        ?assertEqual({reply,{ok,<<"queue-test">>},test_state},
            z_email_server:handle_call({enqueue,<<"queue-test">>,Email,undefined},self(),test_state)),
        [Original] = mnesia:dirty_read(email_queue,<<"queue-test">>),
        Sent = setelement(8,Original,os:timestamp()),
        mnesia:dirty_write(Sent),
        ?assertEqual({reply,{ok,<<"queue-test">>},test_state},
            z_email_server:handle_call({enqueue,<<"queue-test">>,Email,undefined},self(),test_state)),
        ?assertEqual([Sent],mnesia:dirty_read(email_queue,<<"queue-test">>))
    after
        application:stop(mnesia), mnesia:delete_schema([node()]), file:del_dir_r(Dir)
    end.

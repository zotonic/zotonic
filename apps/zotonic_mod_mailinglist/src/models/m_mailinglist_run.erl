%% Copyright 2026 The Zotonic Contributors
%% SPDX-License-Identifier: Apache-2.0
-module(m_mailinglist_run).
-moduledoc("Durable mailing runs and delivery accounting. Template reads require
use of mod_mailinglist, visibility of the page and permission to send to the list.
Recipient addresses and message diagnostics are never published on MQTT.
Internal worker functions require an already authorized sender context.").
-behaviour(zotonic_model).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([m_get/3, create/6, import_scheduled/2, get/2, list/2, allowed/2,
    next_due/1, claim/1, release/2, fail/3, recover/1, cancel/2, resume/2,
    add_recipient/7, transition/4, message/6, prepared/2, refresh/2,
    publish/2, stats/2, previous/5, language/4, next_state/2, status/3, resend/3, fallback/2, rebuild_stats/2]).

-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([<<"run">>, Id | Rest], _Msg, Context) ->
    case get(to_id(Id), Context) of
        {ok, Run} ->
            case allowed(Run, Context) of
                true ->
                    {ok, ByLanguage} = z_db:qmap("select language,status,total from mailinglist_run_stats where run_id=$1 and total>0 order by language,status", [to_id(Id)], Context),
                    Public = maps:without([<<"pickled_context">>,<<"options">>,<<"props">>,<<"request_key">>],Run),
                    {ok, {Public#{ <<"test_address">> => proplists:get_value(single_test_address, maps:get(<<"options">>,Run,[])), <<"stats">> => stats(to_id(Id), Context), <<"languages">> => ByLanguage }, Rest}};
                false -> {error, eacces}
            end;
        _ -> {error, eacces}
    end;
m_get([<<"recipients">>, Id | Rest], Msg, Context) ->
    case get(to_id(Id), Context) of
        {ok, Run} ->
            case allowed(Run, Context) of
                true ->
                    Payload = case Msg of #{payload := P} when is_map(P) -> P; _ -> #{} end,
                    State = maps:get(<<"status">>, Payload, <<>>),
                    After = max(0, to_id(maps:get(<<"after">>, Payload, 0))),
                    {ok, Rows} = z_db:qmap("select id, email, language, status, reason, modified
                        from mailinglist_run_recipient where run_id=$1 and id > $2
                        and ($3 = '' or status=$3) order by id limit 100",
                        [to_id(Id), After, valid_status(State)], Context),
                    {ok, {Rows, Rest}};
                false -> {error, eacces}
            end;
        _ -> {error, eacces}
    end;
m_get([<<"page">>, Id | Rest], _Msg, Context) ->
    {ok, {list({page, m_rsc:rid(Id, Context)}, Context), Rest}};
m_get([<<"list">>, Id | Rest], _Msg, Context) ->
    {ok, {list({list, m_rsc:rid(Id, Context)}, Context), Rest}};
m_get([], #{payload := Filter}, Context) when is_map(Filter) ->
    {ok,{list({filter,Filter},Context),[]}};
m_get([], _Msg, Context) -> {ok, {list(all, Context), []}};
m_get(_, _, _) -> {error, unknown_path}.

to_id(I) when is_integer(I), I >= 0 -> I;
to_id(B) when is_binary(B), byte_size(B) < 20 ->
    try max(0, binary_to_integer(B)) catch _:_ -> 0 end;
to_id(_) -> 0.

valid_status(S) ->
    case lists:member(S, [<<"pending">>, <<"submitting">>, <<"queued">>, <<"retrying">>,
            <<"sent">>, <<"failed">>, <<"bounced">>, <<"skipped">>, <<"cancelled">>]) of
        true -> S;
        false -> <<>>
    end.

-spec get(Id, Context) -> {ok, map()} | {error, term()} when
    Id :: integer(), Context :: z:context().
get(Id, Context) ->
    case z_db:qmap_props_row("select * from mailinglist_run where id=$1", [Id], Context) of
        {ok,Run} ->
            Options = case maps:get(<<"options">>,Run,[]) of L when is_list(L) -> L; _ -> [] end,
            {ok,Run#{<<"options">> => Options}};
        Error -> Error
    end.

-spec allowed(Run, Context) -> boolean() when Run :: map(), Context :: z:context().
allowed(#{<<"page_id">> := Page, <<"mailinglist_id">> := List} = Run, Context) ->
    z_acl:is_allowed(use, mod_mailinglist, Context)
    andalso z_acl:rsc_visible(Page, Context)
    andalso (z_acl:rsc_editable(List, Context)
        orelse (List =:= m_rsc:rid(mailinglist_test, Context)
            andalso is_integer(z_acl:user(Context))
            andalso maps:get(<<"sender_id">>,Run,undefined) =:= z_acl:user(Context)));
allowed(_, _) -> false.

-spec list(Filter, Context) -> [map()] when Filter :: all | tuple(), Context :: z:context().
list(Filter, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) of
        false -> [];
        true ->
            {Where, Args} = case Filter of
                {page, Id} -> {"where page_id=$1", [Id]};
                {list, Id} -> {"where mailinglist_id=$1", [Id]};
                {filter,F} ->
                    State = maps:get(<<"status">>,F,<<>>),
                    Lang = maps:get(<<"language">>,F,<<>>),
                    {"where ($1='' or status=$1) and ($2='' or language=$2) and ($3=0 or page_id=$3) and ($4=0 or mailinglist_id=$4)",
                     [filter_text(State),filter_text(Lang),to_id(maps:get(<<"page_id">>,F,0)),to_id(maps:get(<<"list_id">>,F,0))]};
                all -> {"", []}
            end,
            Offset = case Filter of
                {filter,Fs} -> min(1000000,to_id(maps:get(<<"offset">>,Fs,0)));
                _ -> 0
            end,
            {ok, Rows} = z_db:qmap("select id, page_id, mailinglist_id, sender_id, language,
                fallback_language, status, due, type, created, started, finished, modified,
                error, is_test, parent_id from mailinglist_run " ++ Where ++
                " order by (status in ('preparing','sending','retrying','interrupted')) desc,
                created desc, id desc limit 200 offset " ++ integer_to_list(Offset), Args, Context),
            Visible = [R || R <- Rows, allowed(R,Context)],
            Ids = [maps:get(<<"id">>,R) || R <- Visible],
            Summaries = list_stats(Ids,Context),
            [R#{<<"stats">> => totals(maps:get(maps:get(<<"id">>,R),Summaries,#{}))} || R <- Visible]
    end.

list_stats([],_) -> #{};
list_stats(Ids,Context) ->
    {ok,Rows} = z_db:qmap("select run_id,status,sum(total)::int as total from mailinglist_run_stats
        where run_id=any($1::bigint[]) group by run_id,status",[Ids],Context),
    lists:foldl(fun(#{<<"run_id">> := Id,<<"status">> := S,<<"total">> := N},Acc) ->
        Counts = maps:get(Id,Acc,#{}),
        Acc#{Id => Counts#{S => N}}
    end,#{},Rows).

filter_text(B) when is_binary(B), byte_size(B) < 40 -> B;
filter_text(_) -> <<>>.

-spec create(List, Page, Type, Due, Options, Context) -> {ok, integer()} | {error, term()} when
    List :: integer(), Page :: integer(), Type :: binary(), Due :: calendar:datetime(),
    Options :: list(), Context :: z:context().
create(List, Page, Type, Due, Options, Context) ->
    case mod_mailinglist:is_allowed_to_send(List, Page, Context) of
        false -> {error, eacces};
        true ->
            Lang = proplists:get_value(language, Options, <<>>),
            Fallback = proplists:get_value(fallback_language, Options, fallback(Page,Context)),
            Mode = proplists:get_value(send_mode, Options,
                case proplists:get_bool(is_send_all, Options) of true -> <<"all">>; false -> <<"new">> end),
            Audience = proplists:get_value(audience, Options, <<"matching">>),
            case valid_options(Lang, Fallback, Mode, Audience, Page, Context)
                andalso lists:member(Type, [<<"date">>, <<"publication">>]) of
                false -> {error, invalid_options};
                true ->
                    Id = z_db:q1("insert into mailinglist_run
                        (page_id, mailinglist_id, sender_id, parent_id, language, fallback_language,
                         audience, send_mode, is_test, type, due, props, request_key)
                        values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)
                        on conflict (request_key) do update set request_key=excluded.request_key returning id",
                        [Page, List, z_acl:user(Context), proplists:get_value(parent_id, Options),
                         z_convert:to_binary(Lang), z_convert:to_binary(Fallback), Audience, Mode,
                         List =:= m_rsc:rid(mailinglist_test, Context), Type, Due,
                         ?DB_PROPS([{options, Options}, {pickled_context, z_context:pickle(Context)}]),
                         proplists:get_value(request_key, Options, z_ids:id(32))], Context),
                    publish(Id, Context),
                    {ok, Id}
            end
    end.

-spec fallback(Page, Context) -> atom() when Page :: integer(), Context :: z:context().
fallback(Page,Context) ->
    Languages = available_languages(Page,Context),
    Current = z_context:language(Context),
    case lists:member(Current,Languages) of true -> Current; false -> hd(Languages) end.

valid_options(Lang, Fallback, Mode, Audience, Page, Context) ->
    lists:member(Mode, [<<"new">>, <<"all">>, <<"failed">>])
    andalso lists:member(Audience, [<<"matching">>, <<"all">>])
    andalso is_language(Fallback)
    andalso lists:member(z_convert:to_binary(Fallback), [z_convert:to_binary(L) || L <- available_languages(Page,Context)])
    andalso (Lang =:= <<>> orelse (is_language(Lang) andalso
        lists:member(z_convert:to_binary(Lang), [z_convert:to_binary(L) || L <- available_languages(Page,Context)]))).

available_languages(Page, Context) ->
    case m_rsc:p(Page,language,Context) of
        L when is_list(L), L =/= [] -> L;
        _ -> [z_context:language(Context)]
    end.

is_language(L) when is_atom(L); is_binary(L) ->
    case z_language:to_language_atom(L) of {ok, _} -> true; _ -> false end;
is_language(_) -> false.

%% Old schedules have no reliable run/language history. Preserve their options
%% and sender context; authorization is checked again when they are claimed.
-spec import_scheduled(Row, Context) -> ok when Row :: list(), Context :: z:context().
import_scheduled(Row, Context) ->
    Pickled = proplists:get_value(pickled_context,Row),
    Options = proplists:get_value(options,Row,[]),
    {Sender, Lang} = try z_context:depickle(Pickled) of
        C -> {z_acl:user(C),z_context:language(C)}
    catch _:_ -> {undefined,z_context:language(Context)} end,
    Mode = case proplists:get_bool(is_send_all,Options) of true -> <<"all">>; false -> <<"new">> end,
    z_db:q("insert into mailinglist_run(page_id, mailinglist_id, type, due, props,
        sender_id,fallback_language,send_mode,is_test)
        values ($1,$2,$3,$4,$5,$6,$7,$8,$9)",
        [proplists:get_value(page_id, Row), proplists:get_value(mailinglist_id, Row),
         proplists:get_value(type, Row), proplists:get_value(due, Row),
         ?DB_PROPS([{options,Options},{pickled_context,Pickled}]),Sender,z_convert:to_binary(Lang),Mode,
         proplists:get_value(mailinglist_id,Row) =:= m_rsc:rid(mailinglist_test,Context)], Context),
    ok.

-spec next_due(Context) -> calendar:datetime() | undefined when Context :: z:context().
next_due(Context) ->
    z_db:q1("select min(case when m.type='publication' then greatest(m.due,r.publication_start)
        else m.due end) from mailinglist_run m join rsc r on r.id=m.page_id
        where m.status='scheduled' and (m.type='date' or
            (r.is_published and r.publication_end >= greatest(now(),m.due,r.publication_start)))", Context).

%% Claim atomically before starting a worker, preventing concurrent queue polls
%% from launching the same run twice.
-spec claim(Context) -> {ok,map()} | {error,term()} when Context :: z:context().
claim(Context) ->
    z_db:qmap_props_row("update mailinglist_run set status='preparing',
        started=coalesce(started,now()), modified=now() where id=(
        select m.id from mailinglist_run m join rsc r on r.id=m.page_id
        where m.status='scheduled' and m.due<=now() and
            (m.type='date' or (r.is_published and r.publication_start<=now() and r.publication_end>=now()))
        order by m.due,m.id for update of m skip locked limit 1) returning *", Context).

-spec release(Id, Context) -> ok when Id :: integer(), Context :: z:context().
release(Id, Context) ->
    z_db:q("update mailinglist_run set status='scheduled' where id=$1 and status='preparing'", [Id], Context), ok.

-spec fail(Id, Reason, Context) -> ok when Id :: integer(), Reason :: term(), Context :: z:context().
fail(Id, Reason, Context) ->
    z_db:q("update mailinglist_run set status='failed', error=$2, modified=now(), finished=now()
        where id=$1 and status <> 'cancelled'", [Id, detail(Reason)], Context),
    publish(Id, Context).

-spec recover(Context) -> ok when Context :: z:context().
recover(Context) ->
    Rows = z_db:q("update mailinglist_run set status='interrupted',
        error='Worker interrupted; review pending recipients before resuming.'
        where status in ('preparing','sending') and not prepared
        and modified < now() - interval '10 minutes' returning id", Context),
    lists:foreach(fun({Id}) -> publish(Id, Context) end, Rows),
    %% An ambiguous handoff is never automatically submitted again.
    Stale = z_db:q("update mailinglist_run set status='interrupted',
        error='Email handoff has not been confirmed. Review before retrying.'
        where status in ('sending','retrying') and modified < now() - interval '10 minutes'
        and exists(select 1 from mailinglist_run_recipient rr
            where rr.run_id=mailinglist_run.id and rr.status='submitting')
        returning id", Context),
    lists:foreach(fun({Id}) -> publish(Id, Context) end, Stale), ok.

-spec cancel(Id, Context) -> ok | {error,eacces} when Id :: integer(), Context :: z:context().
cancel(Id, Context) ->
    case get(Id, Context) of
        {ok, R} ->
            case allowed(R, Context) andalso not z_acl:is_read_only(Context) of
                true ->
                    z_db:q("update mailinglist_run set status='cancelled', finished=now(), modified=now()
                        where id=$1 and status not in ('completed','completed_errors','empty','failed')
                        and ((not prepared and not exists(select 1 from mailinglist_run_recipient where run_id=$1)) or exists(select 1 from mailinglist_run_recipient where run_id=$1 and status='pending'))", [Id], Context),
                    Pending = z_db:q("select id from mailinglist_run_recipient where run_id=$1 and status='pending'", [Id], Context),
                    lists:foreach(fun({Rid}) -> transition(Rid, <<"cancelled">>, <<"Cancelled before submission">>, Context) end, Pending),
                    publish(Id, Context), ok;
                false -> {error, eacces}
            end;
        _ -> {error, eacces}
    end.

-spec resume(Id, Context) -> ok | {error,eacces} when Id :: integer(), Context :: z:context().
resume(Id, Context) ->
    case get(Id, Context) of
        {ok, R} ->
            case allowed(R, Context) andalso maps:get(<<"status">>, R) =:= <<"interrupted">>
                andalso ((not maps:get(<<"prepared">>,R) andalso maps:get(<<"total">>,stats(Id,Context),0) =:= 0) orelse maps:get(<<"pending">>,stats(Id,Context),0) > 0)
                andalso mod_mailinglist:is_allowed_to_send(maps:get(<<"mailinglist_id">>,R),maps:get(<<"page_id">>,R),Context) of
                true ->
                    z_db:q("update mailinglist_run set status='scheduled', due=now(), type='date',
                        error=null, modified=now(), props=$2 where id=$1 and status='interrupted'",
                        [Id, ?DB_PROPS([{pickled_context,z_context:pickle(Context)},
                            {options,maps:get(<<"options">>, R, [])}])], Context),
                    mod_mailinglist:ensure_scheduled_task(Context), publish(Id, Context), ok;
                false -> {error, eacces}
            end;
        _ -> {error, eacces}
    end.

-spec add_recipient(Run, Email, Rsc, Lang, State, Reason, Context) -> ok when
    Run :: integer(), Email :: binary(), Rsc :: integer() | undefined, Lang :: binary(),
    State :: binary(), Reason :: binary() | undefined, Context :: z:context().
add_recipient(Run, Email, Rsc, Lang, State, Reason, Context) ->
    z_db:transaction(fun(Ctx) ->
        case z_db:q1("insert into mailinglist_run_recipient(run_id,email,recipient_id,language,status,reason)
            values ($1,$2,$3,$4,$5,$6) on conflict (run_id,email) do nothing returning id",
            [Run,Email,Rsc,Lang,State,Reason], Ctx) of
            undefined -> ok;
            _ -> counter(Run, Lang, State, 1, Ctx)
        end
    end, Context).

counter(Run, Lang, State, Delta, Context) ->
    z_db:q("insert into mailinglist_run_stats(run_id,language,status,total) values ($1,$2,$3,$4)
        on conflict (run_id,language,status) do update set total=mailinglist_run_stats.total+$4",
        [Run,Lang,State,Delta], Context), ok.

-spec transition(Id, State, Reason, Context) -> integer() when
    Id :: integer(), State :: binary(), Reason :: binary() | undefined, Context :: z:context().
transition(Id, State, Reason, Context) ->
    z_db:transaction(fun(Ctx) ->
        {ok, R} = z_db:qmap_row("select * from mailinglist_run_recipient where id=$1 for update", [Id], Ctx),
        Old = maps:get(<<"status">>, R),
        New = next_state(Old, State),
        Run = maps:get(<<"run_id">>, R),
        case Old =:= New of
            true -> ok;
            false ->
                Lang = maps:get(<<"language">>, R),
                %% Increment first: no negative values are ever inserted into the summary table.
                counter(Run, Lang, New, 1, Ctx),
                z_db:q("update mailinglist_run_stats set total=total-1 where run_id=$1 and language=$2 and status=$3",
                    [Run,Lang,Old], Ctx),
                z_db:q("update mailinglist_run_recipient set status=$2, reason=$3, modified=now() where id=$1",
                    [Id,New,Reason], Ctx)
        end,
        Run
    end, Context).

%% Duplicate and delayed notifications must not undo a terminal result.
-spec next_state(binary(), binary()) -> binary().
next_state(<<"bounced">>, _) -> <<"bounced">>;
next_state(<<"cancelled">>, _) -> <<"cancelled">>;
next_state(<<"skipped">>, _) -> <<"skipped">>;
next_state(_, <<"bounced">>) -> <<"bounced">>;
next_state(<<"sent">>, _) -> <<"sent">>;
next_state(<<"failed">>, _) -> <<"failed">>;
next_state(<<"retrying">>, <<"queued">>) -> <<"retrying">>;
next_state(Old, <<"submitting">>) when Old =/= <<"pending">> -> Old;
next_state(_, New) -> New.

-spec message(Id, State, Final, Retry, Detail, Context) -> ok when
    Id :: binary() | undefined, State :: binary(), Final :: boolean(),
    Retry :: integer() | undefined, Detail :: term(), Context :: z:context().
message(undefined, _, _, _, _, _) -> ok;
message(MsgId, State, Final, Retry, Detail, Context) ->
    case z_db:q1("select recipient_id from mailinglist_run_message where message_nr=$1", [MsgId], Context) of
        undefined -> ok;
        Rid ->
            Run = z_db:transaction(fun(Ctx) ->
                RunId = transition(Rid, State, detail(Detail), Ctx),
                z_db:q("update mailinglist_run_message set status=(select status from mailinglist_run_recipient where id=$2),
                    is_final=is_final or $3, retry_count=greatest(retry_count,$4), detail=$5, modified=now()
                    where message_nr=$1", [MsgId,Rid,Final,case Retry of undefined -> 0; _ -> Retry end,detail(Detail)], Ctx),
                RunId
            end, Context),
            refresh(Run, Context)
    end.

detail(eacces) -> <<"The sender no longer has permission to send this mailing.">>;
detail(missing_context) -> <<"The scheduled sender context is missing. Create a new mailing.">>;
detail(invalid_context) -> <<"The scheduled sender context is invalid. Create a new mailing.">>;
detail(undefined) -> undefined;
detail(B) when is_binary(B) -> B;
detail(T) -> iolist_to_binary(io_lib:format("~p", [T])).

-spec stats(Id, Context) -> map() when Id :: integer(), Context :: z:context().
stats(Id, Context) ->
    {ok, Rows} = z_db:qmap("select status,sum(total)::int as total from mailinglist_run_stats where run_id=$1 group by status", [Id], Context),
    Counts = maps:from_list([{maps:get(<<"status">>,R),maps:get(<<"total">>,R)} || R <- Rows]),
    totals(Counts).

totals(Counts) ->
    Total = lists:sum(maps:values(Counts)),
    Selected = Total - maps:get(<<"skipped">>,Counts,0),
    Pending = lists:sum([maps:get(S,Counts,0) || S <- [<<"pending">>,<<"submitting">>,<<"queued">>,<<"retrying">>]]),
    Counts#{ <<"total">> => Total, <<"waiting">> => Pending,
        <<"unsuccessful">> => maps:get(<<"failed">>,Counts,0)+maps:get(<<"bounced">>,Counts,0),
        <<"selected">> => Selected, <<"processed">> => Selected-Pending,
        <<"percent">> => case Selected of 0 -> 0; _ -> (Selected-Pending)*100 div Selected end }.

-spec prepared(Id, Context) -> ok when Id :: integer(), Context :: z:context().
prepared(Id, Context) ->
    z_db:q("update mailinglist_run set prepared=true, modified=now() where id=$1", [Id], Context),
    refresh(Id, Context).

-spec status(binary(), boolean(), map()) -> binary().
status(Old, _, _) when Old =:= <<"cancelled">>; Old =:= <<"failed">> -> Old;
status(<<"interrupted">>, Prepared, S) ->
    case not Prepared orelse maps:get(<<"pending">>,S,0) + maps:get(<<"submitting">>,S,0) > 0 of
        true -> <<"interrupted">>;
        false -> status(<<"sending">>,true,S)
    end;
status(_, false, _) -> <<"sending">>;
status(_, true, S) ->
    Pending = lists:sum([maps:get(K,S,0) || K <- [<<"pending">>,<<"submitting">>,<<"queued">>]]),
    Retry = maps:get(<<"retrying">>,S,0),
    Errors = maps:get(<<"failed">>,S,0)+maps:get(<<"bounced">>,S,0),
    if Pending > 0 -> <<"sending">>; Retry > 0 -> <<"retrying">>;
       Errors > 0 -> <<"completed_errors">>;
       true -> case maps:get(<<"sent">>,S,0) of 0 -> <<"empty">>; _ -> <<"completed">> end end.

-spec refresh(Id, Context) -> ok when Id :: integer(), Context :: z:context().
refresh(Id, Context) ->
    Result = z_db:transaction(fun(Ctx) ->
        {ok, R} = z_db:qmap_row("select status,prepared from mailinglist_run where id=$1 for update", [Id], Ctx),
        Old = maps:get(<<"status">>,R),
        New = status(Old,maps:get(<<"prepared">>,R),stats(Id,Ctx)),
        z_db:q("update mailinglist_run set status=$2::varchar, modified=now(),
            finished=case when $2::varchar in ('completed','completed_errors','empty') then coalesce(finished,now()) else finished end
            where id=$1", [Id,New], Ctx),
        Old =/= New
    end, Context),
    case Result of
        true -> publish(Id, Context);
        _ ->
            case z_db:q1("update mailinglist_run set notified=now() where id=$1
                and (notified is null or notified < now()-interval '2 seconds') returning id", [Id], Context) of
                undefined -> ok;
                _ -> publish(Id, Context)
            end
    end.

-spec publish(Id, Context) -> ok when Id :: integer(), Context :: z:context().
publish(Id, Context) ->
    case z_db:q("select page_id,mailinglist_id from mailinglist_run where id=$1", [Id], Context) of
        [{Page,List}] ->
            lists:foreach(fun(Resource) ->
                z_mqtt:publish([<<"model">>,<<"mailinglist">>,<<"event">>,Resource,<<"runs">>],
                    #{id => Resource}, Context)
            end, lists:usort([Page,List])), ok;
        [] -> ok
    end.

-spec previous(Run, Email, Lang, States, Context) -> boolean() when
    Run :: map(), Email :: binary(), Lang :: binary(), States :: [binary()], Context :: z:context().
previous(Run, Email, Lang, States, Context) ->
    z_db:q1("select exists(select 1 from mailinglist_run_recipient rr
        join mailinglist_run r on r.id=rr.run_id where r.page_id=$1 and r.mailinglist_id=$2
        and r.id<>$3 and rr.email=$4 and rr.language=$5 and rr.status=any($6::varchar[]))",
        [maps:get(<<"page_id">>,Run), maps:get(<<"mailinglist_id">>,Run), maps:get(<<"id">>,Run),
         Email,Lang,States], Context).

%% Resolve once, store the actual rendering language, and use it consistently
%% for resource and email-only subscribers. Never silently choose a translation.
-spec language(binary(), term(), binary(), [atom()]) -> {ok, binary()} | {skip, binary()}.
language(Selected, Preferred, Fallback, Available) ->
    Requested = case Selected of <<>> -> case Preferred of undefined -> Fallback; <<>> -> Fallback; _ -> Preferred end; _ -> Selected end,
    case z_language:to_language_atom(Requested) of
        {ok, Lang} ->
            case lists:member(Lang, Available) of
                true -> {ok,z_convert:to_binary(Lang)};
                false ->
                    Base = z_language:fallback_language(Lang),
                    case lists:member(Base, Available) of
                        true -> {ok,z_convert:to_binary(Base)};
                        false -> {skip,<<"Missing translation">>}
                    end
            end;
        _ -> {skip,<<"Unknown recipient language">>}
    end.

%% Retry/resend retains a link to the original run and its language settings.
-spec resend(Id, Mode, Context) -> {ok,integer()} | {error,term()} when
    Id :: integer(), Mode :: binary(), Context :: z:context().
resend(Id, Mode, Context) when Mode =:= <<"failed">>; Mode =:= <<"all">>; Mode =:= <<"new">> ->
    case get(Id,Context) of
        {ok,R} ->
            case allowed(R,Context) of
                true ->
                    Options = [{single_test_address,proplists:get_value(single_test_address,maps:get(<<"options">>,R,[]))},
                        {parent_id,Id},{send_mode,Mode},
                        {language,maps:get(<<"language">>,R)},
                        {fallback_language,maps:get(<<"fallback_language">>,R)},
                        {audience,maps:get(<<"audience">>,R)}],
                    create(maps:get(<<"mailinglist_id">>,R),maps:get(<<"page_id">>,R),
                        <<"date">>,calendar:universal_time(),Options,Context);
                false -> {error,eacces}
            end;
        _ -> {error,eacces}
    end.

%% Maintenance repair: rebuild summaries from authoritative recipient states.
-spec rebuild_stats(Id, Context) -> ok when Id :: integer(), Context :: z:context().
rebuild_stats(Id, Context) ->
    ok = z_db:transaction(fun(Ctx) ->
        z_db:q("select id from mailinglist_run where id=$1 for update",[Id],Ctx),
        z_db:q("select id from mailinglist_run_recipient where run_id=$1 order by id for update",[Id],Ctx),
        z_db:q("delete from mailinglist_run_stats where run_id=$1",[Id],Ctx),
        z_db:q("insert into mailinglist_run_stats(run_id,language,status,total)
            select run_id,language,status,count(*) from mailinglist_run_recipient where run_id=$1
            group by run_id,language,status",[Id],Ctx), ok
    end,Context),
    refresh(Id,Context).

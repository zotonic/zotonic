%% Copyright 2026 The Zotonic Contributors
%% SPDX-License-Identifier: Apache-2.0
-module(z_mailinglist_run).
-moduledoc("Batch preparation and submission of a durable mailing run. Recipient
snapshots survive worker exits. Messages with an ambiguous queue handoff are
left for review, never automatically submitted a second time.").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([send/2, preview/4, review/4]).

-spec send(Id, Context) -> ok when Id :: integer(), Context :: z:context().
send(Id, Context) ->
    try
        {ok, Run} = m_mailinglist_run:get(Id, Context),
        true = mod_mailinglist:is_allowed_to_send(maps:get(<<"mailinglist_id">>,Run),
            maps:get(<<"page_id">>,Run), Context),
        prepare(Run, Context),
        batches(Run, Context),
        m_mailinglist_run:prepared(Id, Context),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR(#{in => mod_mailinglist, text => <<"Mailing run interrupted">>,
                run_id => Id, result => Class, reason => Reason, stack => Stack}),
            z_db:q("update mailinglist_run set status='interrupted', modified=now(),
                error='Sending was interrupted. Resume to process recipients not yet submitted.'
                where id=$1 and status <> 'cancelled'", [Id], Context),
            m_mailinglist_run:publish(Id, Context),
            ok
    end.

prepare(#{<<"id">> := Id} = Run, Context) ->
    %% A preparation snapshot is committed together. A crash cannot leave half
    %% of the audience frozen while a resumed run silently changes the rest.
    case z_db:q1("select exists(select 1 from mailinglist_run_recipient where run_id=$1)", [Id], Context) of
        true -> ok;
        false ->
            List = maps:get(<<"mailinglist_id">>,Run),
            Recipients = recipients(Run,List,Context),
            RunWithHistory = with_history(Run,Context),
            ok = z_db:transaction(fun(Ctx) ->
                case z_db:q1("select status from mailinglist_run where id=$1 for update",[Id],Ctx) of
                    <<"cancelled">> -> ok;
                    _ -> maps:foreach(fun(Email, Recipient) -> snapshot(RunWithHistory, Email, Recipient, Ctx) end, Recipients)
                end
            end, Context)
    end,
    m_mailinglist_run:prepared(Id, Context).

resolve(Run, Email0, Recipient, Context) ->
    Email = case Email0 of undefined -> <<>>; _ -> z_convert:to_binary(Email0) end,
    {Rsc, Pref} = case Recipient of
        I when is_integer(I) -> {I,m_rsc:p_no_acl(I,pref_language,Context)};
        M when is_map(M) -> {maps:get(<<"rsc_id">>,M,undefined), maps:get(<<"pref_language">>,M,undefined)}
    end,
    Page = maps:get(<<"page_id">>,Run),
    Lang = maps:get(<<"language">>,Run),
    Fallback = maps:get(<<"fallback_language">>,Run),
    Available = case m_rsc:p(Page,language,Context) of L when is_list(L), L =/= [] -> L; _ -> [z_context:language(Context)] end,
    Resolution = m_mailinglist_run:language(Lang,Pref,Fallback,Available),
    {Actual,State,Reason} = case Resolution of
        {skip,Why} -> {Fallback,<<"skipped">>,Why};
        {ok,Lg} ->
            {S,W} = eligibility(Run,Email,Pref,Lg,Available,Context),
            {Lg,S,W}
    end,
    {FinalState,FinalReason} = case Recipient of
        #{ <<"skip_reason">> := SkipReason } -> {<<"skipped">>,SkipReason};
        _ -> {State,Reason}
    end,
    {Email,Rsc,Actual,FinalState,FinalReason}.

snapshot(Run, Email0, Recipient, Context) ->
    {Email,Rsc,Actual,State,Reason} = resolve(Run,Email0,Recipient,Context),
    ok = m_mailinglist_run:add_recipient(maps:get(<<"id">>,Run),Email,Rsc,Actual,State,Reason,Context).

eligibility(Run, Email, Pref, Lang, Available, Context) ->
    Selected = maps:get(<<"language">>,Run),
    Match = case {Selected,maps:get(<<"audience">>,Run)} of
        {<<>>, _} -> true;
        {_, <<"all">>} -> true;
        _ -> m_mailinglist_run:language(<<>>,Pref,maps:get(<<"fallback_language">>,Run),Available) =:= {ok,Lang}
    end,
    case {Email,Match} of
        {<<>>, _} -> {<<"skipped">>,<<"Missing email address">>};
        {_, false} -> {<<"skipped">>,<<"Language mismatch">>};
        _ ->
            case z_email_utils:is_email(Email) of
                false -> {<<"skipped">>,<<"Invalid email address">>};
                true ->
                    case m_email_status:is_ok_to_send(Email,Context) of
                        false -> {<<"skipped">>,<<"Address blocked or suppressed">>};
                        true -> previous(Run,Email,Lang,Context)
                    end
            end
    end.

previous(#{<<"is_test">> := true, <<"send_mode">> := Mode}, _, _, _) when Mode =/= <<"failed">> -> {<<"pending">>,undefined};
previous(#{<<"send_mode">> := <<"all">>}, _, _, _) -> {<<"pending">>,undefined};
previous(Run, Email, Lang, _Context) ->
    {Sent,Failed} = maps:get({Email,Lang},maps:get(history,Run,#{}),{false,false}),
    case {maps:get(<<"send_mode">>,Run),Sent,Failed} of
        {_, true, _} -> {<<"skipped">>,<<"Previously sent or already pending">>};
        {<<"failed">>, false, false} -> {<<"skipped">>,<<"No previous failure">>};
        {<<"new">>, false, true} -> {<<"skipped">>,<<"Previous failure; use retry failed recipients">>};
        _ -> {<<"pending">>,undefined}
    end.

batches(#{<<"id">> := Id} = Run, Context) ->
    case z_db:q1("select status from mailinglist_run where id=$1", [Id], Context) of
        S when S =:= <<"cancelled">>; S =:= <<"interrupted">> -> ok;
        _ ->
            {ok, Rows} = z_db:qmap("select * from mailinglist_run_recipient
                where run_id=$1 and status='pending' order by id limit 100", [Id], Context),
            case Rows of
                [] -> ok;
                _ ->
                    lists:foreach(fun(R) -> submit(Run,R,Context) end,Rows),
                    m_mailinglist_run:refresh(Id,Context),
                    batches(Run,Context)
            end
    end.

submit(Run, Recipient, Context) ->
    Id = maps:get(<<"id">>,Recipient),
    Email = maps:get(<<"email">>,Recipient),
    Lang = maps:get(<<"language">>,Recipient),
    MsgId = z_ids:id(32),
    Reserved = z_db:transaction(fun(Ctx) ->
        %% Serialize duplicate checks for concurrent runs of the same page/list.
        z_db:q("select pg_advisory_xact_lock($1,$2)",
            [maps:get(<<"page_id">>,Run),maps:get(<<"mailinglist_id">>,Run)],Ctx),
        Status = z_db:q1("select status from mailinglist_run where id=$1 for update", [maps:get(<<"id">>,Run)],Ctx),
        Current = z_db:q1("select status from mailinglist_run_recipient where id=$1 for update", [Id],Ctx),
        case {Status,Current} of
            {<<"cancelled">>,_} ->
                m_mailinglist_run:transition(Id,<<"cancelled">>,<<"Cancelled before submission">>,Ctx), false;
            {S,<<"pending">>} when S =:= <<"sending">>; S =:= <<"preparing">> ->
                %% Other runs' pending snapshots must not block each other forever.
                Duplicate = maps:get(<<"send_mode">>,Run) =/= <<"all">>
                    andalso not maps:get(<<"is_test">>,Run)
                    andalso m_mailinglist_run:previous(Run,Email,Lang,
                        [<<"submitting">>,<<"queued">>,<<"retrying">>,<<"sent">>],Ctx),
                case Duplicate of
                    true -> m_mailinglist_run:transition(Id,<<"skipped">>,<<"Previously sent or already pending">>,Ctx), false;
                    false ->
                        m_mailinglist_run:transition(Id,<<"submitting">>,undefined,Ctx),
                        z_db:q("insert into mailinglist_run_message(message_nr,recipient_id) values ($1,$2)", [MsgId,Id],Ctx),
                        true
                end;
            _ -> false
        end
    end,Context),
    case Reserved of
        true ->
            List = maps:get(<<"mailinglist_id">>,Run),
            Page = maps:get(<<"page_id">>,Run),
            Rsc = maps:get(<<"recipient_id">>,Recipient),
            KeyRecipient = case Rsc of undefined -> Email; _ -> Rsc end,
            {ok,Key} = z_mailinglist_recipients:recipient_key_encode(KeyRecipient,List,Context),
            From = m_mailinglist:get_email_from(List,Context),
            Mail = #email{to=Email, from=From, queue=true, html_tpl={cat,"mailing_page.tpl"},
                vars=[{id,Page},{list_id,List},{email_from,From},{recipient_id,Rsc},
                      {recipient_key,Key},{email,Email},{email_language,Lang},{mailinglist_run_id,maps:get(<<"id">>,Run)}],
                attachments=m_edge:objects(Page,hasdocument,Context)},
            case z_email_server:send_queued(MsgId,Mail,z_context:set_language(Lang,Context)) of
                {ok,_} -> m_mailinglist_run:message(MsgId,<<"queued">>,false,0,undefined,Context);
                {error,Reason} -> m_mailinglist_run:message(MsgId,<<"failed">>,true,0,Reason,Context)
            end;
        false -> ok;
        Other -> error({reserve_failed,Other})
    end.

recipients(#{<<"is_test">> := true, <<"options">> := Options}, List, Context) ->
    case proplists:get_value(single_test_address,Options) of
        undefined -> normalized_candidates(List,Context);
        Email -> #{Email => #{<<"email">> => Email}}
    end;
recipients(_, List, Context) -> normalized_candidates(List,Context).

normalized_candidates(List,Context) ->
    maps:fold(fun(Email,R,Acc) ->
        Key = case m_mailinglist:normalize_email(Email) of
            undefined -> z_convert:to_binary(Email);
            E -> E
        end,
        Acc#{Key => R}
    end,#{},z_mailinglist_recipients:list_candidates(List,Context)).

%% Review uses the same eligibility logic as sending. Counts are estimates:
%% subscriptions and content may change before a scheduled run actually starts.
-spec preview(List, Page, Options, Context) -> map() when
    List :: integer(), Page :: integer(), Options :: list(), Context :: z:context().
preview(List,Page,Options,Context) ->
    maps:get(counts,review(List,Page,Options,Context)).

%% Summarize exclusions without exposing recipient addresses in the review.
-spec review(List, Page, Options, Context) -> map() when
    List :: integer(), Page :: integer(), Options :: list(), Context :: z:context().
review(List,Page,Options,Context) ->
    true = mod_mailinglist:is_allowed_to_send(List,Page,Context),
    Run = #{<<"id">> => 0, <<"page_id">> => Page, <<"mailinglist_id">> => List,
        <<"is_test">> => List =:= m_rsc:rid(mailinglist_test,Context),
        <<"options">> => Options, <<"parent_id">> => proplists:get_value(parent_id,Options),
        <<"language">> => proplists:get_value(language,Options,<<>>),
        <<"fallback_language">> => proplists:get_value(fallback_language,Options,m_mailinglist_run:fallback(Page,Context)),
        <<"audience">> => proplists:get_value(audience,Options,<<"matching">>),
        <<"send_mode">> => proplists:get_value(send_mode,Options,<<"new">>)},
    RunWithHistory = with_history(Run,Context),
    maps:fold(fun(Email,R,Acc) ->
        {_,_,Lang,State,Reason} = resolve(RunWithHistory,Email,R,Context),
        Counts = maps:update_with({Lang,State},fun(N) -> N+1 end,1,maps:get(counts,Acc)),
        Reasons = case State of
            <<"skipped">> -> maps:update_with(Reason,fun(N) -> N+1 end,1,maps:get(reasons,Acc));
            _ -> maps:get(reasons,Acc)
        end,
        Acc#{counts => Counts,reasons => Reasons}
    end,#{counts => #{},reasons => #{}},recipients(Run,List,Context)).

%% Resolve history in one query, rather than two queries per candidate in the
%% interactive estimate and snapshot. Submission still checks under a lock.
with_history(Run,Context) ->
    {ok,Rows} = z_db:qmap("select rr.email,rr.language,
        bool_or(rr.status in ('sent','pending','submitting','queued','retrying')) as sent,
        bool_or(rr.status in ('failed','bounced') and ($4::bigint is null or rr.run_id=$4)) as failed
        from mailinglist_run_recipient rr join mailinglist_run r on r.id=rr.run_id
        where r.page_id=$1 and r.mailinglist_id=$2 and r.id<>$3
        group by rr.email,rr.language",
        [maps:get(<<"page_id">>,Run),maps:get(<<"mailinglist_id">>,Run),maps:get(<<"id">>,Run),
         case maps:get(<<"send_mode">>,Run) of
             <<"failed">> -> maps:get(<<"parent_id">>,Run,undefined);
             _ -> undefined
         end],Context),
    History = maps:from_list([{{maps:get(<<"email">>,R),maps:get(<<"language">>,R)},
        {maps:get(<<"sent">>,R),maps:get(<<"failed">>,R)}} || R <- Rows]),
    Run#{history => History}.

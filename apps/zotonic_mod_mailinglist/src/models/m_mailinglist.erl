%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Mailinglist model.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(m_mailinglist).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get_stats/2,
    get_enabled_recipients/2,
    list_recipients/2,
    count_recipients/2,

    recipient_status/3,

    insert_recipients/4,
    insert_recipient/4,
    insert_recipient/5,

    insert_recipient_rsc/3,

    update_recipient/3,

    recipient_get/2,
    recipient_get/3,
    recipient_delete/2,
    recipient_delete/3,
    recipient_delete_quiet/2,
    recipient_confirm/2,
    recipient_is_enabled_toggle/2,
    recipients_clear/2,

    list_subscriptions_by_email/2,
    list_subscriptions_by_rsc_id/2,
    list_subscriptions_by_rsc_id/3,

    insert_scheduled/3,
    insert_scheduled/4,
    delete_scheduled/3,
    get_scheduled/2,
    check_scheduled/1,

    get_email_from/2,
    get_recipients_by_email/2,
    reset_log_email/3,

    recipient_set_operation/4,

    normalize_email/1,

    periodic_cleanup/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type welcome_message_type() :: send_confirm | send_welcome | silent.
-export_type([ welcome_message_type/0 ]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"stats">>, MailingId | Rest ], _Msg, Context) ->
    case z_acl:rsc_editable(MailingId, Context) of
        true -> {ok, {get_stats(MailingId, Context), Rest}};
        false -> {ok, {{undefined, undefined}, Rest}}
    end;
m_get([ <<"rsc_stats">>, RscId | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) of
        true -> {ok, {get_rsc_stats(RscId, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"recipient">>, RecipientId | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) of
        true -> {ok, {recipient_get(z_convert:to_integer(RecipientId), Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"scheduled">>, RscId | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(RscId, Context) of
        true -> {ok, {get_scheduled(RscId, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"confirm_key">>, ConfirmKey | Rest ], _Msg, Context) ->
    {ok, {get_confirm_key(ConfirmKey, Context), Rest}};
m_get([ <<"subscription">>, ListId, Email | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) of
        true -> {ok, {recipient_get(ListId, Email, Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"subscriptions">> ], _Msg, Context) ->
    {ok, L} = list_subscriptions_by_rsc_id(z_acl:user(Context), Context),
    {ok, {L, []}};
m_get([ <<"subscriptions">>, <<"key">>, undefined | Rest ], _Msg, Context) ->
    {ok, L} = list_subscriptions_by_rsc_id(z_acl:user(Context), Context),
    {ok, {L, Rest}};
m_get([ <<"subscriptions">>, <<"key">>, Key | Rest ], _Msg, Context) when is_binary(Key) ->
    case z_mailinglist_recipients:recipient_key_decode(Key, Context) of
        {ok, #{
            recipient := Email,
            list_id := _ListId
        }} when is_binary(Email) ->
            {ok, L} = list_subscriptions_by_email(Key, Context),
            {ok, {L, Rest}};
        {ok, #{
            recipient := RscId,
            list_id := ListId
        }} when is_integer(RscId) ->
            case list_subscriptions_by_rsc_id(RscId, ListId, Context) of
                {ok, L} ->
                    {ok, {L, Rest}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
m_get([ <<"subscriptions">>, RscId | Rest ], _Msg, Context) when is_integer(RscId) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context)
        orelse z_acl:user(Context) =:= RscId
        orelse z_acl:rsc_editable(RscId, Context)
    of
        true ->
            {ok, List} = list_subscriptions_by_rsc_id(RscId, Context),
            {ok, {List, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"subscriptions">>, Key | Rest ], _Msg, Context) when is_binary(Key) ->
    case binary:match(Key, <<"@">>) of
        {_, _} ->
            case z_acl:is_allowed(use, mod_mailinglist, Context) of
                true ->
                    {ok, List} = list_subscriptions_by_email(Key, Context),
                    {ok, {List, Rest}};
                false ->
                    {error, eacces}
            end;
        nomatch ->
            case m_rsc:rid(Key, Context) of
                undefined ->
                    {ok, {[], Rest}};
                RscId ->
                    case z_acl:is_allowed(use, mod_mailinglist, Context)
                        orelse z_acl:user(Context) =:= RscId
                        orelse z_acl:rsc_editable(RscId, Context)
                    of
                        true ->
                            {ok, List} = list_subscriptions_by_rsc_id(RscId, Context),
                            {ok, {List, Rest}};
                        false ->
                            {error, eacces}
                    end
            end
    end;
m_get([ <<"subscriptions">>, _Key | Rest ], _Msg, _Context) ->
    {ok, {[], Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Get the stats for the mailing. Number of recipients and list of scheduled resources.
-spec get_stats( m_rsc:resource_id(), z:context() ) -> map().
get_stats(ListId, Context) ->
    Counts = z_mailinglist_recipients:count_recipients(ListId, Context),
    Scheduled = z_db:q("
        select page_id
        from mailinglist_scheduled
                join rsc on id = page_id
        where mailinglist_id = $1
        order by publication_start", [ListId], Context),
    Counts#{
        scheduled => Scheduled
    }.


%% @doc Get the stats for all mailing lists which have been sent to a rsc (content_id)
-spec get_rsc_stats( m_rsc:resource_id(), z:context() ) -> [ {ListId::m_rsc:resource_id(), Statuslist} ]
    when Statuslist :: list( binary() ).
get_rsc_stats(Id, Context) ->
    F = fun() ->
        RsLog = z_db:q("
            select other_id, min(created) as sent_on, count(distinct(envelop_to))
            from log_email
            where content_id = $1
            group by other_id",
            [Id],
            Context),

        Stats = lists:map(
                    fun({ListId, Created, Total}) ->
                        {ListId, [{created, Created}, {total, Total}]}
                    end,
                    RsLog),
        %% merge in all mailer statuses
        PerStatus = z_db:q("
            select other_id, mailer_status, count(envelop_to)
            from log_email
            where content_id = $1
            group by other_id, mailer_status",
            [Id],
            Context),

        lists:foldl(
            fun({ListId, Status, Count}, St) ->
                z_utils:prop_replace(ListId,
                                     [{z_convert:to_atom(Status), Count}|proplists:get_value(ListId, St, [])],
                                     St)
            end,
            Stats,
            PerStatus)
    end,
    z_depcache:memo(F, {mailinglist_stats, Id}, 1, [Id], Context). %% Cache a little while to prevent database DOS while mail is sending



%% @doc Fetch all enabled recipients from a list.
-spec get_enabled_recipients( m_rsc:resource_id(), z:context() ) -> list( binary() ).
get_enabled_recipients(ListId, Context) ->
    Emails = z_db:q("
        select email
        from mailinglist_recipient
        where mailinglist_id = $1
          and is_enabled = true", [ z_convert:to_integer(ListId) ], Context),
    [ E || {E} <- Emails ].


%% @doc List all recipients of a mailinglist (as maps with binary keys, props expanded)
-spec list_recipients( m_rsc:resource_id(), z:context() ) -> {ok, list( map() )} | {error, term()}.
list_recipients(ListId, Context) ->
    z_db:qmap_props("
        select *
        from mailinglist_recipient
        where mailinglist_id = $1",
        [ z_convert:to_integer(ListId) ], Context).

-spec count_recipients( m_rsc:resource_id(), z:context() ) -> non_neg_integer().
count_recipients(ListId, Context) ->
    z_db:q1("
            select count(*)
            from mailinglist_recipient
            where mailinglist_id = $1",
            [ z_convert:to_integer(ListId) ],
            Context).


%% @doc Toggle the enabled flag of a recipient
recipient_is_enabled_toggle(RecipientId, Context) ->
    case z_db:q("
            update mailinglist_recipient
            set is_enabled = not is_enabled
            where id = $1", [ z_convert:to_integer(RecipientId) ], Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Fetch the recipient record for the recipient id.
-spec recipient_get(RecipientId, Context) -> undefined | RecipientProps when
    RecipientId :: integer() | binary() | string(),
    Context :: z:context(),
    RecipientProps :: proplists:proplist().
recipient_get(RecipientId, Context) ->
    z_db:assoc_row("
        select *
        from mailinglist_recipient
        where id = $1",
        [ z_convert:to_integer(RecipientId) ],
        Context).

%% @doc Fetch the recipient record by e-mail address
-spec recipient_get(ListId, Email, Context) -> undefined | RecipientProps when
    ListId :: undefined | m_rsc:resource(),
    Email :: binary() | string(),
    Context :: z:context(),
    RecipientProps :: proplists:proplist().
recipient_get(undefined, _Email, _Context) ->
    undefined;
recipient_get(<<>>, _Email, _Context) ->
    undefined;
recipient_get(ListId, Email, Context) ->
    Email1 = normalize_email(Email),
    z_db:assoc_row("
        select * from mailinglist_recipient
        where mailinglist_id = $1
          and email = $2",
        [ m_rsc:rid(ListId, Context), Email1 ], Context).


%% @doc Delete a recipient without sending the recipient a goodbye e-mail.
recipient_delete_quiet(RecipientId, Context) ->
    case recipient_get(RecipientId, Context) of
        undefined -> {error, enoent};
        RecipientProps -> recipient_delete1(RecipientProps, true, Context)
    end.

%% @doc Delete a recipient and send the recipient a goodbye e-mail.
recipient_delete(RecipientId, Context) ->
    case recipient_get(RecipientId, Context) of
        undefined -> {error, enoent};
        RecipientProps -> recipient_delete1(RecipientProps, false, Context)
    end.

%% @doc Delete a recipient by list id and email
recipient_delete(ListId, Email, Context) ->
    case recipient_get(ListId, Email, Context) of
        undefined -> {error, enoent};
        RecipientProps -> recipient_delete1(RecipientProps, false, Context)
    end.

recipient_delete1(RecipientProps, IsQuiet, Context) ->
    RecipientId = proplists:get_value(id, RecipientProps),
    z_db:delete(mailinglist_recipient, RecipientId, Context),
    ListId = proplists:get_value(mailinglist_id, RecipientProps),
    if
        IsQuiet == false ->
            z_notifier:notify1(
                #mailinglist_message{
                    what = send_goodbye,
                    list_id = ListId,
                    recipient = RecipientProps
                }, Context);
        true ->
            ok
    end,
    ok.

%% @doc Confirm the recipient with the given unique confirmation key.
-spec recipient_confirm( binary(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
recipient_confirm(ConfirmKey, Context) ->
    case z_db:q_row("select id, is_enabled, mailinglist_id from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context) of
        {RecipientId, _IsEnabled, ListId} ->
            NewConfirmKey = z_ids:id(20),
            z_db:q("update mailinglist_recipient set confirm_key = $2, is_enabled = true where confirm_key = $1", [ConfirmKey, NewConfirmKey], Context),
            z_notifier:notify(#mailinglist_message{
                    what = send_welcome,
                    list_id = ListId,
                    recipient = RecipientId
                }, Context),
            {ok, RecipientId};
        undefined ->
            {error, enoent}
    end.

%% @doc Clear all recipients of the list
-spec recipients_clear( m_rsc:resource_id(), z:context() ) -> ok.
recipients_clear(ListId, Context) ->
    %% TODO clear person edges to list
    z_db:q("delete from mailinglist_recipient where mailinglist_id = $1", [ListId], Context),
    ok.

%% @doc Fetch the information for a confirmation key
-spec get_confirm_key( binary(), z:context() ) -> proplists:proplist() | undefined.
get_confirm_key(ConfirmKey, Context) ->
    z_db:assoc_row("select id, mailinglist_id, email, confirm_key from mailinglist_recipient where confirm_key = $1", [ConfirmKey], Context).


%% @doc Check if the email address is on the mailinglist, returns true for enabled and disabled entries.
-spec recipient_status(ListId, Email, Context) -> Status when
    ListId :: m_rsc:resource(),
    Email :: binary() | string() | undefined,
    Context :: z:context(),
    Status :: subscribed | enoent | unsubscribed.
recipient_status(ListId, Email, Context) ->
    case z_db:q1("
        select is_enabled
        from mailinglist_recipient
        where email = $1
          and mailinglist_id = $2",
        [ normalize_email(Email), m_rsc:rid(ListId, Context) ],
        Context)
    of
        undefined -> enoent;
        true -> subscribed;
        false -> unsubscribed
    end.


%% @doc Insert a recipient in the mailing list, send a message to the recipient when needed. The
%% current user must have link permission on the recipient to add the subscriberof edge.
-spec insert_recipient_rsc(ListId, UserId, Context) -> ok | {error, Reason} when
    ListId :: m_rsc:resource(),
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: exsubscriberof | enoent | eacces | term().
insert_recipient_rsc(ListId, UserId, Context) ->
    case z_acl:rsc_visible(ListId, Context) of
        false ->
            {error, eacces};
        true ->
            case m_edge:get_id(UserId, exsubscriberof, ListId, Context) of
                undefined ->
                    case m_edge:insert(UserId, subscriberof, ListId, Context) of
                        {ok, _} ->
                            ok;
                        {error, _} = Error ->
                            Error
                    end;
                _EdgeId ->
                    {error, exsubscriberof}
            end
    end.


%% @doc Insert a recipient in the mailing list, send a message to the recipient when needed.
-spec insert_recipient(ListId, Email, WelcomeMessageType, Context) -> ok | {error, Reason} when
    ListId :: m_rsc:resource(),
    Email :: binary() | string(),
    WelcomeMessageType :: welcome_message_type(),
    Context :: z:context(),
    Reason :: enoent | eacces.
insert_recipient(ListId, Email, WelcomeMessageType, Context) ->
    insert_recipient(ListId, Email, [], WelcomeMessageType, Context).

-spec insert_recipient(ListId, Email, Props, WelcomeMessageType, Context) -> ok | {error, Reason} when
    ListId :: m_rsc:resource(),
    Email :: binary() | string(),
    Props :: proplists:proplist(),
    WelcomeMessageType :: welcome_message_type(),
    Context :: z:context(),
    Reason :: enoent | eacces.
insert_recipient(ListId, Email, Props, WelcomeMessageType, Context) ->
    case m_rsc:rid(ListId, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            insert_recipient_1(RscId, Email, Props, WelcomeMessageType, Context)
    end.

insert_recipient_1(ListId, Email, Props, WelcomeMessageType, Context) ->
    case z_acl:rsc_visible(ListId, Context) of
        false ->
            {error, eacces};
        true ->
            Email1 = normalize_email(Email),
            Rec = z_db:q_row("select id, is_enabled, confirm_key
                              from mailinglist_recipient
                              where mailinglist_id = $1
                                and email = $2", [ListId, Email1], Context),
            ConfirmKey = z_ids:id(20),
            {RecipientId, WelcomeMessageType1} = case Rec of
                {RcptId, true, _OldConfirmKey} ->
                    %% Present and enabled
                    {RcptId, silent};
                {RcptId, false, OldConfirmKey} ->
                    %% Present, but not enabled
                    NewConfirmKey = case OldConfirmKey of
                        undefined -> ConfirmKey;
                        _ -> OldConfirmKey
                    end,
                    case WelcomeMessageType of
                        send_confirm ->
                            case NewConfirmKey of
                                OldConfirmKey -> nop;
                                _ -> z_db:q("update mailinglist_recipient
                                             set confirm_key = $2
                                             where id = $1", [RcptId, NewConfirmKey], Context)
                            end,
                            {RcptId, {send_confirm, NewConfirmKey}};
                        _ ->
                            z_db:q("update mailinglist_recipient
                                    set is_enabled = true,
                                        confirm_key = $2
                                    where id = $1", [RcptId, NewConfirmKey], Context),
                            {RcptId, WelcomeMessageType}
                    end;
                undefined ->
                    %% Not present
                    IsEnabled = case WelcomeMessageType of
                        send_welcome -> true;
                        send_confirm -> false;
                        silent -> true
                    end,
                    Cols = [
                        {mailinglist_id, ListId},
                        {is_enabled, IsEnabled},
                        {email, Email1},
                        {confirm_key, ConfirmKey}
                    ] ++ [ {K, case is_list(V) of true-> z_convert:to_binary(V); false -> V end} || {K,V} <- Props ],
                    {ok, RcptId} = z_db:insert(mailinglist_recipient, Cols, Context),
                    {RcptId, WelcomeMessageType}
            end,
            case WelcomeMessageType1 of
                silent ->
                    nop;
                _ -> z_notifier:notify(
                        #mailinglist_message{
                            what = WelcomeMessageType1,
                            list_id = ListId,
                            recipient = RecipientId
                        }, Context)
            end,
            ok
    end.


%% @doc Update a single recipient; changing e-mail address or name details.
update_recipient(RcptId, Props, Context) ->
    {ok, _} = z_db:update(mailinglist_recipient, RcptId, Props, Context),
    ok.


%% @doc Replace all recipients of the mailinglist. Do not send welcome messages to the recipients.
-spec insert_recipients(ListId, Recipients, IsTruncate, Context) -> ok | {error, Reason} when
    ListId :: m_rsc:resource_id(),
    Recipients :: list( binary() | string() )
                | binary(),
    IsTruncate::boolean(),
    Context :: z:context(),
    Reason :: enoent | eacces | term().
insert_recipients(ListId, Bin, IsTruncate, Context) when is_binary(Bin) ->
    Lines = z_string:split_lines(Bin),
    Rcpts = lines_to_recipients(Lines),
    insert_recipients(ListId, Rcpts, IsTruncate, Context);
insert_recipients(ListId, Recipients, IsTruncate, Context) ->
    case m_rsc:rid(ListId, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            insert_recipients_1(RscId, Recipients, IsTruncate, Context)
    end.

insert_recipients_1(ListId, Recipients, IsTruncate, Context) ->
    case z_acl:rsc_editable(ListId, Context) of
        true ->
            ok = z_db:transaction(
                            fun(Ctx) ->
                                {ok, Now} = insert_recipients_2(ListId, Recipients, Ctx),
                                optional_truncate(ListId, IsTruncate, Now, Ctx)
                            end, Context);
        false ->
            {error, eacces}
    end.

insert_recipients_2(ListId, Recipients, Context) ->
    Now = erlang:universaltime(),
    [ replace_recipient(ListId, R, Now, Context) || R <- Recipients ],
    {ok, Now}.

optional_truncate(_, false, _, _) ->
    ok;
optional_truncate(ListId, true, Now, Context) ->
    z_db:q("
        delete from mailinglist_recipient
        where mailinglist_id = $1
          and timestamp < $2", [ListId, Now], Context),
    ok.

replace_recipient(ListId, Recipient, Now, Context) when is_binary(Recipient) ->
    replace_recipient(ListId, Recipient, [], Now, Context);
replace_recipient(ListId, Recipient, Now, Context) ->
    replace_recipient(ListId, proplists:get_value(email, Recipient), proplists:delete(email, Recipient), Now, Context).


replace_recipient(ListId, Email, Props, Now, Context) ->
    case m_rsc:rid(ListId, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            replace_recipient_1(RscId, Email, Props, Now, Context)
    end.

replace_recipient_1(ListId, Email, Props, Now, Context) ->
    case normalize_email(Email) of
        <<>> ->
            skip;
        Email1 ->
            case z_db:q1("select id from mailinglist_recipient where mailinglist_id = $1 and email = $2",
                         [ListId, Email1], Context) of
                undefined ->
                    ConfirmKey = z_ids:id(20),
                    Props1 = [{confirm_key, ConfirmKey},
                              {email, Email1},
                              {timestamp, Now},
                              {mailinglist_id, ListId},
                              {is_enabled, true}] ++ Props,
                    z_db:insert(mailinglist_recipient, Props1, Context);
                EmailId ->
                    z_db:update(mailinglist_recipient, EmailId, [{timestamp, Now}, {is_enabled, true}] ++ Props, Context)
            end
    end.


lines_to_recipients(Lines) ->
    lines_to_recipients(Lines, []).

lines_to_recipients([], Acc) ->
    Acc;
lines_to_recipients([Line|Lines], Acc) ->
    %% Split every line on tab
    Trimmed = z_string:trim( z_convert:to_binary(Line) ),
    case z_csv_parser:parse_line(Trimmed, 9) of
        {ok, [<<>>]} ->
            lines_to_recipients(Lines, Acc);
        {ok, Row} ->
            R = line_to_recipient(Row),
            lines_to_recipients(Lines, [R|Acc])
    end.

line_to_recipient([ Email ]) ->
    [
        {email, Email}
    ];
line_to_recipient([ Email, NameFirst ]) ->
    [
        {email, Email},
        {name_first, NameFirst}
    ];
line_to_recipient([ Email, NameFirst, NameLast ]) ->
    [
        {email, Email},
        {name_first, NameFirst},
        {name_surname, NameLast}
    ];
line_to_recipient([ Email, NameFirst, NameLast, NamePrefix | _ ]) ->
    [
        {email, Email},
        {name_first, NameFirst},
        {name_surname, NameLast},
        {name_surname_prefix, NamePrefix}
    ].


%% @doc Get all mailingslists for this resource, checking both the recipients and the
%% subscriberof edges. The found subscriptions must have the email address as the resource's primary email address.
-spec list_subscriptions_by_rsc_id(RscId, ListId, Context ) -> {ok, Subscriptions} | {error, Reason} when
    RscId :: m_rsc:resource_id(),
    ListId :: m_rsc:resource_id() | undefined,
    Context :: z:context(),
    Subscriptions :: [ map() ],
    Reason :: enoent.
list_subscriptions_by_rsc_id(RscId, ListId, Context) ->
    case list_subscriptions_by_rsc_id(RscId, Context) of
        {ok, L} ->
            % Optionally append the query-based list for which the user was selected.
            case list_single_subscription(RscId, ListId, Context) of
                {ok, #{
                    <<"mailinglist_id">> := MId
                } = Sub} ->
                    case lists:any(
                        fun
                            (#{
                                <<"rsc_id">> := RId1,
                                <<"mailinglist_id">> := MId1
                            }) ->
                                RId1 =:= RscId andalso MId =:= MId1;
                            (_) ->
                                false
                        end,
                        L)
                    of
                        true -> {ok, L};
                        false -> {ok, [ Sub | L ]}
                    end;
                {error, _} ->
                    {ok, L}
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Get all mailingslists for this resource, checking both the recipients and the
%% subscriberof edges. The found subscriptions must have the email address as the resource's primary email address.
-spec list_subscriptions_by_rsc_id( RscId, Context ) -> {ok, Subscriptions} | {error, Reason} when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Subscriptions :: [ map() ],
    Reason :: enoent.
list_subscriptions_by_rsc_id(RscId0, Context) ->
    case m_rsc:rid(RscId0, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            MIds = m_edge:objects(RscId, subscriberof, Context),
            ViaRsc = lists:map(
                fun(MId) ->
                    Email1 = normalize_email(m_rsc:p_no_acl(RscId, <<"email_raw">>, Context)),
                    #{
                        <<"title">> => m_rsc:p_no_acl(MId, <<"title">>, Context),
                        <<"summary">> => m_rsc:p_no_acl(MId, <<"summary">>, Context),
                        <<"rsc_id">> => RscId,
                        <<"mailinglist_id">> => MId,
                        <<"recipient_id">> => undefined,
                        <<"email">> => Email1,
                        <<"pref_language">> => m_rsc:p_no_acl(RscId, <<"pref_language">>, Context),
                        <<"is_mailinglist_private">> => z_convert:to_bool(m_rsc:p_no_acl(MId, <<"mailinglist_private">>, Context))
                    }
                end,
                MIds),
            % Fetch all verified email addresses of this user
            Idns = m_identity:get_rsc_by_type(RscId, email, Context),
            Emails = lists:filtermap(
                fun(Idn) ->
                    case proplists:get_value(is_verified, Idn) of
                        true ->
                            {true, proplists:get_value(key, Idn)};
                        false ->
                            false
                    end
                end,
                Idns),
            ViaEmail = lists:flatten(lists:map(fun(E) -> subscriptions_by_email_1(E, Context) end, Emails)),
            {ok, subs_merge_sort(ViaEmail ++ ViaRsc)}
    end.

%% @doc Get all mailingslists with this email address, checking both the recipients and the
%% subscriberof edges. The found resources must have the email address as their primary email address.
-spec list_subscriptions_by_email( Email, Context ) -> {ok, Subscriptions} when
    Email :: binary() | string(),
    Context :: z:context(),
    Subscriptions :: [ map() ].
list_subscriptions_by_email(Email, Context) ->
    Email1 = normalize_email(Email),
    SubscriberOf = m_rsc:rid(subscriberof, Context),
    Pairs = z_db:q("
        select e.subject_id, e.object_id
        from edge e
            join identity idn
                on idn.type = 'email'
                and idn.key = $1
                and e.subject_id = idn.rsc_id
        where e.predicate_id = $2",
        [ Email1, SubscriberOf ],
        Context),
    ViaRsc = lists:filtermap(
        fun({RscId, MId}) ->
            E = m_rsc:p_no_acl(RscId, <<"email_raw">>, Context),
            case normalize_email(E) of
                Email1 ->
                    Recipient = #{
                        <<"title">> => m_rsc:p_no_acl(MId, <<"title">>, Context),
                        <<"summary">> => m_rsc:p_no_acl(MId, <<"summary">>, Context),
                        <<"rsc_id">> => RscId,
                        <<"mailinglist_id">> => MId,
                        <<"email">> => Email1,
                        <<"pref_language">> => m_rsc:p_no_acl(RscId, <<"pref_language">>, Context),
                        <<"is_mailinglist_private">> => z_convert:to_bool(m_rsc:p_no_acl(MId, <<"mailinglist_private">>, Context))
                    },
                    {true, Recipient};
                _ ->
                    false
            end
        end,
        Pairs),
    ViaMailings = subscriptions_by_email_1(Email1, Context),
    {ok, subs_merge_sort(ViaMailings ++ ViaRsc)}.

subs_merge_sort(L) ->
    M = lists:foldl(
        fun(#{ <<"mailinglist_id">> := MId } = Sub, Acc) ->
            case Acc of
                #{ MId := AccSub } ->
                    Acc#{
                        MId => maps:merge(Sub, AccSub)
                    };
                _ ->
                    Acc#{
                        MId => Sub
                    }
            end
        end,
        #{},
        L),
    lists:sort(fun cmp_title/2, maps:values(M)).

subscriptions_by_email_1(NormalizedEmail, Context) ->
    {ok, ViaMailings} = z_db:qmap_props("
        select *
        from mailinglist_recipient
        where email = $1
          and is_enabled",
        [ NormalizedEmail ],
        Context),
    lists:map(
        fun(R) ->
            MId = maps:get(<<"mailinglist_id">>, R),
            #{
                <<"title">> => m_rsc:p_no_acl(MId, <<"title">>, Context),
                <<"summary">> => m_rsc:p_no_acl(MId, <<"summary">>, Context),
                <<"mailinglist_id">> => MId,
                <<"recipient_id">> => maps:get(<<"id">>, R),
                <<"email">> => NormalizedEmail,
                <<"pref_language">> => maps:get(<<"pref_language">>, R, undefined),
                <<"is_mailinglist_private">> => z_convert:to_bool(m_rsc:p_no_acl(MId, <<"mailinglist_private">>, Context))
            }
        end,
        ViaMailings).

cmp_title(A, B) ->
    maps:get(<<"title">>, A) =< maps:get(<<"title">>, B).


-spec list_single_subscription(RscId, ListId, Context) -> {ok, Subscription} | {error, Reason} when
    RscId :: m_rsc:resource_id() | undefined,
    ListId :: m_rsc:resource_id() | undefined,
    Context :: z:context(),
    Subscription :: map(),
    Reason :: enoent | nolist.
list_single_subscription(_, undefined, _Context) ->
    {error, enoent};
list_single_subscription(undefined, _, _Context) ->
    {error, enoent};
list_single_subscription(RscId, ListId, Context) ->
    case m_rsc:is_a(ListId, mailinglist, Context) of
        true ->
            case m_edge:get_id(RscId, exsubscriberof, ListId, Context) of
                undefined ->
                    E = m_rsc:p_no_acl(RscId, <<"email_raw">>, Context),
                    Subscription = #{
                        <<"title">> => m_rsc:p_no_acl(ListId, <<"title">>, Context),
                        <<"summary">> => m_rsc:p_no_acl(ListId, <<"summary">>, Context),
                        <<"rsc_id">> => RscId,
                        <<"mailinglist_id">> => ListId,
                        <<"email">> => E,
                        <<"pref_language">> => m_rsc:p_no_acl(RscId, <<"pref_language">>, Context),
                        <<"is_mailinglist_private">> => z_convert:to_bool(m_rsc:p_no_acl(ListId, <<"mailinglist_private">>, Context))
                    },
                    {ok, Subscription};
                _EdgeId ->
                    {error, exsubscriberof}
            end;
        false ->
            {error, nolist}
    end.

%% @doc Insert a mailing to be send when the page becomes visible
insert_scheduled(ListId, PageId, Context) ->
    insert_scheduled(ListId, PageId, [], Context).

%% @doc Insert a mailing to be send when the page becomes visible
insert_scheduled(ListId, PageId, Options, Context) ->
    true = z_acl:rsc_editable(ListId, Context),
    Exists = z_db:q1("
                select count(*)
                from mailinglist_scheduled
                where page_id = $1 and mailinglist_id = $2", [PageId,ListId], Context),
    case Exists of
        0 ->
           z_mqtt:publish(
                [ <<"model">>, <<"mailinglist">>, <<"event">>, ListId, <<"scheduled">> ],
                #{ id => ListId, page_id => PageId, action => <<"insert">> },
                Context);
        1 ->
            nop
    end,
    z_db:q("
        insert into mailinglist_scheduled
            (page_id, mailinglist_id, props)
        values
            ($1, $2, $3)
        on conflict (page_id, mailinglist_id)
        do update set props = $3
        ",
        [ PageId, ListId, ?DB_PROPS([ {options, Options} ]) ],
        Context),
    ok.


%% @doc Delete a scheduled mailing
delete_scheduled(ListId, PageId, Context) ->
    true = z_acl:rsc_editable(ListId, Context),
    case z_db:q("
            delete from mailinglist_scheduled
            where page_id = $1
              and mailinglist_id = $2",
            [PageId,ListId],
            Context)
    of
        0 ->
            0;
        N when N > 0 ->
            z_mqtt:publish(
                [ <<"model">>, <<"mailinglist">>, <<"event">>, ListId, <<"scheduled">> ],
                #{ id => ListId, page_id => PageId, action => <<"delete">> },
                Context),
            N
    end.


%% @doc Get the list of scheduled mailings for a page.
-spec get_scheduled(PageId, Context) -> [ ListId ] when
    PageId :: m_rsc:resource_id(),
    Context :: z:context(),
    ListId :: m_rsc:resource_id().
get_scheduled(PageId, Context) ->
    Lists = z_db:q("
        select mailinglist_id
        from mailinglist_scheduled
        where page_id = $1", [PageId], Context),
    [ ListId || {ListId} <- Lists ].


%% @doc Fetch the next scheduled mailing that are published and in the publication date range.
-spec check_scheduled(Context) -> undefined | Mailing when
    Context :: z:context(),
    Mailing :: { ListId, PageId, Options },
    ListId :: m_rsc:resource_id(),
    PageId :: m_rsc:resource_id(),
    Options :: mod_mailinglist:mailing_options().
check_scheduled(Context) ->
    case z_db:assoc_props_row("
        select m.*
        from mailinglist_scheduled m
        where (
            select r.is_published
               and r.publication_start <= now()
               and r.publication_end >= now()
            from rsc r
            where r.id = m.mailinglist_id
        )
        limit 1", Context)
    of
        undefined ->
            undefined;
        Row ->
            {
                proplists:get_value(mailinglist_id, Row),
                proplists:get_value(page_id, Row),
                proplists:get_value(options, Row, [])
            }
    end.


%% @doc Reset the email log for given list/page combination, allowing one to send the same page again to the given list.
reset_log_email(ListId, PageId, Context) ->
    z_db:q("delete from log_email where other_id = $1 and content_id = $2", [ListId, PageId], Context),
    z_depcache:flush({mailinglist_stats, PageId}, Context),
    z_mqtt:publish(
        [ <<"model">>, <<"mailinglist">>, <<"event">>, ListId, <<"scheduled">> ],
        #{ id => ListId, page_id => PageId, action => <<"reset">> },
        Context),
    ok.


%% @doc Get the "from" address used for this mailing list. Looks first in the mailinglist rsc for a ' mailinglist_reply_to' field; falls back to site.email_from config variable.
get_email_from(ListId, Context) ->
    FromEmail = case m_rsc:p(ListId, <<"mailinglist_reply_to">>, Context) of
                    Empty when Empty =:= undefined; Empty =:= <<>> ->
                        z_convert:to_binary(m_config:get_value(site, email_from, Context));
                    RT ->
                        z_convert:to_binary(RT)
                end,
    FromName = case m_rsc:p(ListId, <<"mailinglist_sender_name">>, Context) of
                  undefined -> <<>>;
                  <<>> -> <<>>;
                  SenderName -> z_convert:to_binary(SenderName)
               end,
    z_email:combine_name_email(FromName, FromEmail).



%% @doc Get all recipients with this email address.
get_recipients_by_email(Email, Context) ->
    Email1 = normalize_email(Email),
    [ Id || {Id} <- z_db:q("SELECT id FROM mailinglist_recipient WHERE email = $1", [Email1], Context) ].


%% @doc Perform a set operation on two lists. The result of the
%% operation gets stored in the first list.
recipient_set_operation(Op, IdA, IdB, Context) when
        Op =:= union;
        Op =:= subtract;
        Op =:= intersection ->
    A = get_email_set(IdA, Context),
    B = get_email_set(IdB, Context),
    Emails = sets:to_list(sets:Op(A, B)),
    insert_recipients(IdA, Emails, true, Context).


get_email_set(ListId, Context) ->
    Es = z_db:q("
        SELECT email
        FROM mailinglist_recipient
        WHERE mailinglist_id = $1
          AND is_enabled", [ListId], Context),
    Normalized = lists:map(
        fun({E}) -> normalize_email(E) end,
        Es),
    sets:from_list(Normalized).

normalize_email(undefined) ->
    undefined;
normalize_email(Email) ->
    Email1 = m_identity:normalize_key(<<"email">>, Email),
    case z_email_utils:is_email(Email1) of
        true -> Email1;
        false -> undefined
    end.

%% @doc Periodically remove bouncing and disabled addresses from the mailinglist.
%% Due to the GDPR and privacy in general we delete unused email addresses.
-spec periodic_cleanup(z:context()) -> ok.
periodic_cleanup(Context) ->
    % Remove disabled entries that were not updated for more than 3 months
    z_db:q("
        delete from mailinglist_recipient
        where not is_enabled
          and timestamp < now() - interval '3 months'",
        Context),
    % Remove entries that are invalid, blocked or bouncing
    MaybeBouncing = z_db:q("
        select r.email
        from mailinglist_recipient r
             join email_status s
             on r.email = s.email
        where s.modified < now() - interval '3 months'
            and (  not s.is_valid
                or s.is_blocked
                or s.bounce >= s.modified
                or s.error >= s.modified)
        ",
        Context,
        300000),
    Invalid = lists:filtermap(
        fun({Email}) ->
            case m_email_status:is_ok_to_send(Email, Context) of
                true -> false;
                false -> {true, Email}
            end
        end,
        MaybeBouncing),
    z_db:q("
        delete from mailinglist_recipient
        where email = any($1)
        ",
        [ Invalid ],
        Context),
    ok.

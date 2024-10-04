%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2024 Marc Worrell
%% @doc Model for registering the status per email recipient.
%% @end

%% Copyright 2015-2024 Marc Worrell
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


-module(m_email_status).

-behaviour (zotonic_model).

-export([
    m_get/3,

    is_valid/2,
    is_ok_to_send/2,
    is_blocked/2,
    get/2,

    clear_status/3,
    clear_status/2,

    block/2,

    mark_received/2,

    mark_read/2,
    mark_sent/3,
    mark_failed/4,
    mark_bounced/2,

    periodic_cleanup/1,

    install/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"is_valid">>, Email | Rest ], _Msg, Context) ->
    {ok, {is_valid(Email, Context), Rest}};
m_get([ Email | Rest ], _Msg, Context) ->
    case is_allowed(Email, Context) of
        true ->
            {ok, {get(Email, Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


is_allowed(Email, Context) ->
    z_acl:is_allowed(use, mod_email_status, Context)
    orelse z_acl:is_admin(Context)
    orelse is_user_email(Email, Context).

%% @doc Check if an email address is an email address of the current user. Must
%% match the primary email address or one of the email identities.
is_user_email(Email, Context) ->
    case z_acl:user(Context) of
        undefined -> false;
        UserId ->
            m_rsc:p_no_acl(UserId, email, Context) =:= Email
            orelse m_rsc:p_no_acl(UserId, email_raw, Context) =:= Email
            orelse is_visible_email(Email, Context)
    end.

is_visible_email(Email, Context) ->
    Idns = m_identity:lookup_by_type_and_key_multi(email, Email, Context),
    lists:any(
        fun(Idn) ->
            RscId = proplists:get_value(rsc_id, Idn),
            z_acl:rsc_editable(RscId, Context)
        end,
        Idns).


%% @doc Block an email address. No email will be sent to this address and no
%% email will be received from this address.
-spec block(Email, Context) -> ok when
    Email :: binary(),
    Context :: z:context().
block(Email0, Context) ->
    Email = normalize(Email0),
    case z_db:q("
        update email_status
        set is_valid = false,
            is_blocked = true
        where email = $1",
        [Email],
        Context)
    of
        1 ->
            maybe_notify(Email, false, false, true, true, Context),
            ok;
        0 ->
            z_db:q("
                insert into email_status
                    (email, is_valid, is_blocked)
                values
                    ($1, false, true)
                ",
                [Email],
                Context),
            ok
    end.

%% @doc Clear the status of the email address. Returns an error if the
%% the given email address is not an identity of the resource.
-spec clear_status(Id, Email, Context) -> ok | {error, notfound} when
    Id :: m_rsc:resource() | undefined,
    Email :: binary(),
    Context :: z:context().
clear_status(undefined, Email0, Context) ->
    clear_status(normalize(Email0), Context);
clear_status(Id, Email0, Context) ->
    Email = normalize(Email0),
    Idns = m_identity:get_rsc_by_type(Id, email, Context),
    Es = [ proplists:get_value(key, Idn) || Idn <- Idns ],
    case lists:member(Email, Es) of
        true ->
            clear_status(Email, Context);
        false ->
            {error, notfound}
    end.

clear_status(Email, Context) ->
    case z_db:q("update email_status
            set is_valid = true,
                is_blocked = false,
                recent_error = null,
                recent_error_ct = 0,
                modified = now()
            where email = $1
              and (is_valid = false or is_blocked or recent_error_ct > 0)",
            [Email],
            Context)
    of
        1 ->
            maybe_notify(Email, false, true, true, true, Context),
            ok;
        0 -> ok
    end.

%% @doc Check if an email address is blocked.
-spec is_blocked(Email, Context) -> IsBlocked when
    Email :: binary(),
    Context :: z:context(),
    IsBlocked :: boolean().
is_blocked(Email, Context) ->
    {_IsValid, _IsOkToSend, IsBlocked} = is_valid_cached(Email, Context),
    IsBlocked.


%% @doc Check if an email address is known to be valid, if nothing known then assume it is valid.
-spec is_ok_to_send(Email, Context) -> IsOkToSend when
    Email :: binary(),
    Context :: z:context(),
    IsOkToSend :: boolean().
is_ok_to_send(Email, Context) ->
    {_IsValid, IsOkToSend, _IsBlocked} = is_valid_cached(Email, Context),
    IsOkToSend.

%% @doc Check if an email address is known to be valid, if nothing known then assume it is valid.
-spec is_valid(Email, Context) -> IsValid when
    Email :: binary(),
    Context :: z:context(),
    IsValid :: boolean().
is_valid(Email, Context) ->
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_cached(Email, Context),
    IsValid.

-spec is_valid_cached(Email, Context) -> {IsValid, IsOkToSend, IsBlocked} when
    Email :: binary(),
    Context :: z:context(),
    IsValid :: boolean(),
    IsOkToSend :: boolean(),
    IsBlocked :: boolean().
is_valid_cached(Email0, Context) ->
    Email = normalize(Email0),
    z_depcache:memo(fun() -> is_valid_nocache(Email, Context) end,
                    {email_is_valid, Email},
                    ?DAY,
                    Context).

-spec is_valid_nocache(Email, Context) -> {IsValid, IsOkToSend, IsBlocked} when
    Email :: binary(),
    Context :: z:context(),
    IsValid :: boolean(),
    IsOkToSend :: boolean(),
    IsBlocked :: boolean().
is_valid_nocache(Email, Context) ->
    case z_db:q("select is_valid, is_blocked, recent_error, recent_error_ct, error_is_final
                 from email_status where email = $1",
                [Email],
                Context)
    of
        [] -> {true, true, false};
        [{_IsValid, true, _RecentError, _RecentErrorCt, _ErrorIsFinal}] -> {false, false, true};
        [{true, _IsBlocked, _RecentError, _RecentErrorCt, _ErrorIsFinal}] -> {true, true, false};
        [{false, _IsBlocked, undefined, _RecentErrorCt, _ErrorIsFinal}] -> {false, true, false};
        [{false, _IsBlocked, _RecentError, _RecentErrorCt, false}] -> {false, true, false};
        [{false, _IsBlocked, _RecentError, RecentErrorCt, true}] -> {false, RecentErrorCt < 5, false}
    end.


%% @doc Fetch the stats from an email address
-spec get(Email, Context) -> EmailStatus | undefined when
    Email :: binary(),
    Context :: z:context(),
    EmailStatus :: proplists:proplist().
get(Email0, Context) ->
    z_db:assoc_row("select * from email_status where email = $1",
                   [normalize(Email0)],
                   Context).


%% @doc Increment the email receive counter. This doesn't say anything about the validity
%%      of the email address, only that we received an email with this envelope address.
-spec mark_received(Email, Context) -> ok when
    Email :: binary(),
    Context :: z:context().
mark_received(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    case z_db:q("
            update email_status
            set is_valid = true,
                receive = now(),
                receive_ct = read_ct + 1,
                modified = now()
            where email = $1",
           [Email],
           Context)
    of
        0 ->
            z_db:q("
                insert into email_status(email, receive, receive_ct)
                values ($1, now(), 1)
                ",
                [Email],
                Context),
            ok;
        1 ->
            maybe_notify(Email, IsValid, true, true, false, Context),
            ok
    end.

%% @doc Mark an email read, typically because a link in the email is followed.
-spec mark_read(Email, Context) -> ok when
    Email :: binary(),
    Context :: z:context().
mark_read(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    case z_db:q("
            update email_status
            set is_valid = not is_blocked,
                read = now(),
                read_ct = read_ct + 1,
                recent_error_ct = 0,
                recent_error = null,
                modified = now()
            where email = $1",
           [Email],
           Context)
    of
        0 ->
            z_db:q("
                insert into email_status(email, read, read_ct)
                values ($1, now(), 1)
                ",
                [Email],
                Context),
            ok;
        1 ->
            maybe_notify(Email, IsValid, true, true, false, Context),
            ok
    end.

%% @doc Track email sending, a sent is marked 'final' if there wasn't a bounce within 4 hours.
-spec mark_sent(Email, IsFinal, Context) -> ok when
    Email :: binary(),
    IsFinal :: boolean(),
    Context :: z:context().
mark_sent(Email0, false, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    case z_db:q("
            update email_status
            set sent = now(),
                sent_ct = sent_ct + 1,
                modified = now()
            where email = $1",
           [Email],
           Context)
    of
        0 ->
            z_db:q("
                insert into email_status(email, sent, sent_ct)
                values ($1, now(), 1)
                ",
                [Email],
                Context),
            ok;
        1 ->
            maybe_notify(Email, IsValid, true, false, false, Context),
            ok
    end;
mark_sent(Email0, true, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    case z_db:q("
        update email_status
        set is_valid = not is_blocked,
            recent_error_ct = 0,
            recent_error = null,
            modified = now()
        where email = $1
          and (bounce is null or bounce < sent)
          and (error < sent or not error_is_final)",
        [Email],
        Context)
    of
        1 ->
            maybe_notify(Email, IsValid, true, true, false, Context),
            ok;
        0 ->
            ok
    end.

%% @doc Mark as failed, happens on connection errors or on errors returned by the receiving smtp server.
%% The final flag is set if our smtp server received a permanent error or gave up sending the email.
%% Keep a counter (recent_error_ct) and date (recent_error) which are updated at most once a day.
-spec mark_failed(Email, IsFinal, Status, Context) -> ok when
    Email :: binary(),
    IsFinal :: boolean(),
    Status :: binary() | {error, term()} | undefined,
    Context :: z:context().
mark_failed(Email0, IsFinal, Status, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    Status1 = z_string:truncatechars(to_binary(Status), 490),
    z_db:transaction(
                fun(Ctx) ->
                    case z_db:q("
                        select recent_error
                        from email_status
                        where email = $1",
                        [Email],
                        Ctx)
                    of
                        [] ->
                            z_db:q(
                                "insert into email_status
                                    (email, is_valid, error_is_final, error, error_status,
                                     error_ct, recent_error_ct, recent_error, modified)
                                 values ($1, false, $2, now(), $3, 1, $4, now(), now())",
                                [Email, IsFinal, Status1, 1],
                                Ctx);
                        [{LastRecent}] ->
                            {RecentDelta, RecentDate} = new_recent_error(LastRecent, IsFinal, Status1),
                            z_db:q("
                                update email_status
                                set is_valid = false,
                                    error = now(),
                                    error_is_final = $2,
                                    error_status = $3,
                                    error_ct = error_ct + 1,
                                    recent_error_ct = recent_error_ct + $4,
                                    recent_error = $5,
                                    modified = now()
                                where email = $1",
                                [Email, IsFinal, Status1, RecentDelta, RecentDate],
                                Ctx),
                            ok
                    end
               end,
               Context),
    maybe_notify(Email, IsValid, false, IsFinal, false, Context).


new_recent_error(LastRecent, IsFinal, Status) ->
    Now = calendar:universal_time(),
    Yesterday = z_datetime:prev_day(Now),
    case IsFinal andalso is_unrecoverable_error(Status) of
        true -> {5, Now}; % Force to give up
        false when LastRecent =:= undefined -> {1, Now};
        false when LastRecent > Yesterday -> {0, LastRecent};
        false when not IsFinal -> {0, LastRecent};
        false when IsFinal -> {1, Now}
    end.


% If the permanent error is about an unknown user then we consider it fruitless to
% try again later.  Errors about missing domains, connection errors and timeouts
% could all be fixed later, so it is ok to mail those for a while.
is_unrecoverable_error(<<"605", _/binary>>) ->
    % mailgun - not trying again as they blocked the email address
    true;
is_unrecoverable_error(<<"550", _/binary>> = Status) ->
    S = z_string:to_lower(Status),
    binary:match(S, <<"mailbox unavailable">>) =/= nomatch          % hotmail
    orelse binary:match(S, <<"does not exist">>) =/= nomatch        % gmail / ziggo
    orelse binary:match(S, <<"access denied">>) =/= nomatch         % outlook
    orelse binary:match(S, <<"unknown user">>) =/= nomatch
    orelse binary:match(S, <<"user unknown">>) =/= nomatch
    orelse binary:match(S, <<"no such user">>) =/= nomatch
    orelse binary:match(S, <<"user invalid">>) =/= nomatch
    orelse binary:match(S, <<"recipient rejected">>) =/= nomatch;
is_unrecoverable_error(_) ->
    false.


to_binary({error, nxdomain}) ->
    <<"Non-Existent Domain">>;
to_binary({error, Reason}) ->
    iolist_to_binary(io_lib:format("~p", [Reason]));
to_binary(T) when is_tuple(T) ->
    iolist_to_binary(io_lib:format("~p", [T]));
to_binary(V) ->
    z_convert:to_binary(V).

%% @doc Mark as bounced, this is handled as permanent error, though it could be temporary if
%% the bounce happened due to some temporary problem on the receiver's end.
-spec mark_bounced(Email, Context) -> ok when
    Email :: binary(),
    Context :: z:context().
mark_bounced(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _IsOkToSend, _IsBlocked} = is_valid_nocache(Email, Context),
    z_db:transaction(
        fun(Ctx)->
            case z_db:q("
                select recent_error
                from email_status
                where email = $1",
                [Email],
                Ctx)
            of
                [] ->
                    % Bounce without a 'sent', ignore.
                    ok;
                [{RecentError}] ->
                    Now = calendar:universal_time(),
                    Yesterday = z_datetime:prev_day(Now),
                    {RecentErrorDelta, RecentError1} = case RecentError of
                        undefined -> {1, Now};
                        D when D > Yesterday -> {0, RecentError};
                        _ -> {1, Now}
                    end,
                    z_db:q("
                        update email_status
                        set is_valid = false,
                            bounce = now(),
                            bounce_ct = bounce_ct + 1,
                            recent_error_ct = recent_error_ct + $2,
                            recent_error = $3,
                            modified = now()
                        where email = $1
                        ",
                        [Email, RecentErrorDelta, RecentError1],
                        Ctx)
            end
        end,
        Context),
    maybe_notify(Email, IsValid, false, true, false, Context),
    ok.

maybe_notify(_Email, IsValid, IsValid, false, _IsManual, _Context) ->
    ok;
maybe_notify(Email, _OldIsValid, IsValid, IsFinal, IsManual, Context) ->
    z_depcache:flush({email_is_valid, Email}, Context),
    z_notifier:notify(#email_status{
                        recipient = Email,
                        is_valid = IsValid,
                        is_final = IsFinal,
                        is_manual = IsManual
                    },
                    Context).


%% @doc Normalize an email address, makes it compatible with the email addresses in m_identity.
normalize(Email) ->
    z_convert:to_binary(m_identity:normalize_key(email, Email)).

%% @doc Periodically delete inactive addresses older than 2 years.
%%      Keep blocked and bouncing entries, to prevent spamming or bad reputation.
-spec periodic_cleanup(Context) -> RowsDeleted when
    Context :: z:context(),
    RowsDeleted :: non_neg_integer().
periodic_cleanup(Context) ->
    z_db:q("
        delete from email_status
        where modified < now() - interval '2 years'
          and not is_blocked
          and (    bounce is null
                or sent is null
                or bounce < sent)
        ",
        Context).

%% @doc Install the email tracking table
install(Context) ->
    case z_db:table_exists(email_status, Context) of
        false ->
            [] = z_db:q("
                create table email_status (
                    email character varying (200) not null,
                    is_valid boolean not null default true,
                    is_blocked boolean not null default false,

                    read timestamp with time zone,
                    read_ct integer not null default 0,

                    receive timestamp with time zone,
                    receive_ct integer not null default 0,

                    sent timestamp with time zone,
                    sent_ct integer not null default 0,

                    recent_error timestamp with time zone,
                    recent_error_ct integer not null default 0,

                    error timestamp with time zone,
                    error_status character varying (500),
                    error_ct integer not null default 0,
                    error_is_final boolean not null default false,

                    bounce timestamp with time zone,
                    bounce_ct integer not null default 0,

                    created timestamp with time zone default current_timestamp not null,
                    modified timestamp with time zone default current_timestamp not null,

                    primary key (email)
                )
                ",
                Context),
            [] = z_db:q("create index email_status_modified on email_status(modified)", Context);
        true ->
            Names = z_db:column_names(email_status, Context),
            case lists:member(recent_error_ct, Names) of
                false ->
                    z_db:q("alter table email_status
                            add column recent_error_ct integer not null default 0,
                            add column recent_error timestamp with time zone",
                           Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    case lists:member(recent_error, Names) of
                        false ->
                            z_db:q("alter table email_status
                                    add column recent_error timestamp with time zone",
                                   Context),
                            z_db:flush(Context),
                            ok;
                        true ->
                            ok
                    end
            end,
            case lists:member(is_blocked, Names) of
                false ->
                    z_db:q("alter table email_status
                            add column is_blocked boolean not null default false",
                           Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end
    end,
    ok.


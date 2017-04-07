%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Model for registering the status per email recipient.

%% Copyright 2015 Marc Worrell
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

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_valid/2,
    is_ok_to_send/2,
    get/2,

    clear_status/3,
    clear_status/2,

    mark_received/2,

    mark_read/2,
    mark_sent/3,
    mark_failed/4,
    mark_bounced/2,

    install/1
    ]).

-include_lib("zotonic.hrl").

m_find_value(is_valid, #m{value=undefined} = M, _Context) ->
    M#m{value=is_valid};
m_find_value(Email, #m{value=is_valid}, Context) when is_binary(Email); is_list(Email) ->
    is_valid(Email, Context);
m_find_value(Email, #m{value=undefined}, Context) when is_binary(Email); is_list(Email) ->
    get(Email, Context);
m_find_value(_X, #m{}, _Context) ->
    undefined.

m_to_list(_m, _Context) ->
    [].

m_value(_m, _Context) ->
    undefined.

%% @doc Clear the status of the email address
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
                recent_error = null,
                recent_error_ct = 0,
                modified = now()
            where email = $1
              and (is_valid = false or recent_error_ct > 0)",
            [Email],
            Context)
    of
        1 ->
            maybe_notify(Email, false, true, true, Context),
            ok;
        0 -> ok
    end.


%% @doc Check if an email address is known to be valid, if nothing known then assume it is valid.
-spec is_ok_to_send(binary(), #context{}) -> boolean().
is_ok_to_send(Email, Context) ->
    {_IsValid, IsOkToSend} = is_valid_cached(Email, Context),
    IsOkToSend.

%% @doc Check if an email address is known to be valid, if nothing known then assume it is valid.
-spec is_valid(binary(), #context{}) -> boolean().
is_valid(Email, Context) ->
    {IsValid, _IsOkToSend} = is_valid_cached(Email, Context),
    IsValid.

-spec is_valid_cached(Email::binary(), #context{}) -> {IsValid::boolean(), IsOkToSend::boolean()}.
is_valid_cached(Email0, Context) ->
    Email = normalize(Email0),
    z_depcache:memo(fun() -> is_valid_nocache(Email, Context) end,
                    {email_valid, Email},
                    ?DAY,
                    Context).

-spec is_valid_nocache(Email::binary(), #context{}) -> {IsValid::boolean(), IsOkToSend::boolean()}.
is_valid_nocache(Email, Context) ->
    case z_db:q("select is_valid, recent_error, recent_error_ct, error_is_final
                 from email_status where email = $1", 
                [Email],
                Context)
    of
        [] -> {true, true};
        [{true, _, _, _}] -> {true, true};
        [{false, undefined, _, _}] -> {false, true};
        [{false, _, _RecentErrorCt, false}] -> {false, true};
        [{false, _, RecentErrorCt, true}] -> {false, RecentErrorCt < 5}
    end.


%% @doc Fetch the stats from an email address
-spec get(binary(), #context{}) -> list() | undefined.
get(Email0, Context) ->
    z_db:assoc_row("select * from email_status where email = $1",
                   [normalize(Email0)],
                   Context).


%% @doc Increment the email receive counter. This doesn't say anything about the validity
%%      of the email address, only that we received an email with this envelope address.
-spec mark_received(binary(), #context{}) -> ok.
mark_received(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
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
            maybe_notify(Email, IsValid, true, true, Context),
            ok
    end.

%% @doc Mark an email read, typically because a link in the email is followed.
-spec mark_read(binary(), #context{}) -> ok.
mark_read(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
    case z_db:q("
            update email_status
            set is_valid = true,
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
            maybe_notify(Email, IsValid, true, true, Context),
            ok
    end.

%% @doc Track email sending, a sent is marked 'final' if there wasn't a bounce within 4 hours.
-spec mark_sent(binary(), boolean(), #context{}) -> ok.
mark_sent(Email0, false, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
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
            maybe_notify(Email, IsValid, true, false, Context),
            ok
    end;
mark_sent(Email0, true, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
    case z_db:q("
        update email_status
        set is_valid = true,
            recent_error_ct = 0,
            recent_error = null,
            modified = now()
        where email = $1
          and bounce < sent
          and (error < sent or not error_is_final)",
        [Email],
        Context)
    of
        1 ->
            maybe_notify(Email, IsValid, true, true, Context),
            ok;
        0 ->
            ok
    end.

%% @doc Mark as failed, happens on connection errors or on errors returned by the receiving smtp server.
%% The final flag is set if our smtp server received a permanent error or gave up sending the email.
%% Keep a counter (recent_error_ct) and date (recent_error) which are updated at most once a day.
-spec mark_failed(binary(), boolean(), binary()|{error,term()}|undefined, #context{}) -> ok.
mark_failed(Email0, IsFinal, Status, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
    Status1 = z_string:truncate(to_binary(Status), 490),
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
    maybe_notify(Email, IsValid, false, IsFinal, Context).


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
is_unrecoverable_error(<<"550", _/binary>> = Status) ->
    S = z_string:to_lower(Status),
    binary:match(S, <<"mailbox unavailable">>) =/= nomatch          % hotmail
    orelse binary:match(S, <<"does not exist">>) =/= nomatch        % gmail / ziggo
    orelse binary:match(S, <<"unknown user">>) =/= nomatch
    orelse binary:match(S, <<"user unknown">>) =/= nomatch
    orelse binary:match(S, <<"no such user">>) =/= nomatch
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
-spec mark_bounced(binary(), #context{}) -> ok.
mark_bounced(Email0, Context) ->
    Email = normalize(Email0),
    {IsValid, _} = is_valid_nocache(Email, Context),
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
    maybe_notify(Email, IsValid, false, true, Context),
    ok.

maybe_notify(_Email, IsValid, IsValid, false, _Context) ->
    ok;
maybe_notify(Email, _OldIsValid, IsValid, IsFinal, Context) ->
    z_depcache:flush({email_valid, Email}, Context),
    z_notifier:notify(#email_status{
                        recipient=Email,
                        is_valid=IsValid,
                        is_final=IsFinal
                    },
                    Context).


%% @doc Normalize an email address, makes it compatible with the email addresses in m_identity.
normalize(Email) ->
    z_convert:to_binary(m_identity:normalize_key(email, Email)).

%% @doc Install the email tracking table
install(Context) ->
    case z_db:table_exists(email_status, Context) of
        false ->
            [] = z_db:q("
                create table email_status (
                    email character varying (200) not null,
                    is_valid boolean not null default true,

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
            end
    end,
    ok.


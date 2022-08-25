%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% Date: 2009-04-25
%%
%% @doc Manage identities of users.  An identity can be an username/password, openid, oauth credentials etc.

%% Copyright 2009-2013 Marc Worrell
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

-module(m_identity).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

-type password() :: iodata().
-type bcrypt_hash() :: {bcrypt, binary()}.
-type sha1_salted_hash() :: {hash, binary(), binary()}.
-type hash() :: bcrypt_hash() | sha1_salted_hash().

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_user/2,
    get_username/1,
    get_username/2,
    get_user_info/1,
    get_user_info/2,
    delete_username/2,
    set_username/3,
    set_username_pw/4,
    ensure_username_pw/2,
    check_username_pw/3,
    check_username_pw/4,
    hash/1,
    needs_rehash/1,
    hash_is_equal/2,
    get/2,
    get_rsc/2,
    get_rsc_by_type/3,
    get_rsc/3,

    is_valid_key/3,
    normalize_key/2,

	lookup_by_username/2,
	lookup_by_verify_key/2,
    lookup_by_type_and_key/3,
    lookup_by_type_and_key_multi/3,

    lookup_by_rememberme_token/2,
    get_rememberme_token/2,
    reset_rememberme_token/2,

	set_by_type/4,
	set_by_type/5,
	delete_by_type/3,
    delete_by_type_and_key/4,

    insert/4,
    insert/5,
    insert_unique/4,
    insert_unique/5,

	set_verify_key/2,
    set_verified/2,
    set_verified/4,
    is_verified/2,

    set_visited/2,

    delete/2,
    merge/3,
    is_reserved_name/1,
    is_peer_whitelisted/1,
    is_ip_whitelisted/2
]).

-export([
    generate_username/2
]).

-include_lib("zotonic.hrl").


%% Default duration and random variance interval for password checks.
%% This prevents a timing difference between checks for existing and
%% non existing accounts.
-define(DEFAULT_PW_CHECK_DURATION, 280).
-define(DEFAULT_PW_CHECK_VARIANCE, 40).


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(lookup, #m{value=undefined} = M, _Context) ->
    M#m{value=lookup};
m_find_value(Type, #m{value=lookup} = M, _Context) ->
    M#m{value={lookup, Type}};
m_find_value(Key, #m{value={lookup, Type}}, Context) ->
    lookup_by_type_and_key_multi(Type, Key, Context);

m_find_value(Id, #m{value=undefined} = M, _Context) ->
    M#m{value=Id};
m_find_value(is_user, #m{value=RscId}, Context) ->
    is_user(RscId, Context);
m_find_value(username, #m{value=RscId}, Context) ->
    get_username(RscId, Context);
m_find_value(user_info, #m{value=RscId}, Context) ->
    get_user_info(RscId, Context);
m_find_value(all, #m{value=RscId} = M, _Context) ->
    M#m{value={all, RscId}};
m_find_value(all_types, #m{value=RscId}, Context) ->
    get_rsc_types(RscId, Context);
m_find_value(Type, #m{value={all,RscId}}, Context) ->
    get_rsc_by_type(RscId, Type, Context);
m_find_value(get, #m{value=undefined} = M, _Context) ->
    M#m{value=get};
m_find_value(IdnId, #m{value=get}, Context) ->
    get(IdnId, Context);
m_find_value(Type, #m{value=RscId}, Context) ->
    get_rsc(RscId, Type, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value={all, RscId}}, Context) ->
    get_rsc(RscId, Context);
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=V}, _Context) ->
    V.


%% @doc Check if the resource has any credentials that will make him/her an user
-spec is_user(m_rsc:resource(), #context{}) -> boolean().
is_user(Id, Context) ->
    case z_db:q1("select count(*) from identity where rsc_id = $1 and type in ('username_pw', 'openid')", [m_rsc:rid(Id, Context)], Context) of
        0 -> false;
        _ -> true
    end.

%% @doc Return the username of the current user
%% @spec get_username(Context) -> Username | undefined
get_username(Context) ->
	case z_acl:user(Context) of
		undefined -> undefined;
		UserId -> get_username(UserId, Context)
	end.

%% @doc Return the username of the resource id, undefined if no username
%% @spec get_username(ResourceId, Context) -> Username | undefined
-spec get_username(m_rsc:resource(), #context{}) -> binary() | undefined.
get_username(Id, Context) ->
    z_db:q1("select key from identity where rsc_id = $1 and type = 'username_pw'", [m_rsc:rid(Id, Context)], Context).


%% @doc Return the username and last login of the current user.
-spec get_user_info(z:context()) -> map().
get_user_info(Context) ->
    case z_acl:user(Context) of
        undefined ->
            #{
                <<"user_id">> => undefined,
                <<"username">> => undefined,
                <<"visited">> => undefined,
                <<"modified">> => undefined,
                <<"is_expired">> => false
            };
        UserId ->
            get_user_info(UserId, Context)
    end.

%% @doc Return the username and last login of the resource id, undefined if no username
-spec get_user_info(m_rsc:resource_id(), z:context()) -> binary().
get_user_info(RscId, Context) when is_integer(RscId) ->
    Row = z_db:q_row("
             select key, visited, prop1, modified
             from identity
             where rsc_id = $1
               and type = 'username_pw'",
            [m_rsc:rid(RscId, Context)],
            Context),
    case Row of
        undefined ->
            #{
                <<"user_id">> => RscId,
                <<"username">> => undefined,
                <<"visited">> => undefined,
                <<"modified">> => undefined,
                <<"is_expired">> => false
            };
        {Key, Visited, Prop1, Modified} ->
            #{
                <<"user_id">> => RscId,
                <<"username">> => Key,
                <<"visited">> => Visited,
                <<"modified">> => Modified,
                <<"is_expired">> => Prop1 =:= <<"expired">>
            }
    end.

%% @doc Check if the user is allowed to change the username of a resource.
-spec is_allowed_set_username( m_rsc:resource_id(), z:context() ) -> boolean().
is_allowed_set_username(Id, Context) when is_integer(Id) ->
    z_acl:is_admin(Context)
    orelse z_acl:is_allowed(use, mod_admin_identity, Context)
    orelse (z_acl:is_allowed(update, Id, Context) andalso Id =:= z_acl:user(Context)).


%% @doc Delete an username from a resource.
-spec delete_username(m_rsc:resource() | undefined, z:context()) -> ok | {error, eacces | enoent}.
delete_username(undefined, _Context) ->
    {error, enoent};
delete_username(1, _Context) ->
    throw({error, admin_username_cannot_be_deleted});
delete_username(Id, Context) when is_integer(Id) ->
    case is_allowed_set_username(Id, Context)  of
        true ->
            z_db:q("delete from identity where rsc_id = $1 and type = 'username_pw'", [ Id, Context ], Context),
            z_mqtt:publish(["~site", "rsc", Id, "identity"], {identity, <<"username_pw">>}, Context),
            ok;
        false ->
            {error, eacces}
    end;
delete_username(Id, Context) ->
    delete_username( m_rsc:rid(Id, Context), Context ).

%% @doc Change the username of the resource id, only possible if there is already an username/password set
%% @spec set_username(ResourceId, Username, Context) -> ok | {error, Reason}
set_username(undefined, _Username, _Context) ->
    {error, enoent};
set_username(Id, Username, Context) when is_integer(Id) ->
    case is_allowed_set_username(Id, Context) of
        true ->
            Username1 = z_string:to_lower(Username),
            case is_reserved_name(Username1) of
                true ->
                    {error, eexist};
                false ->
                    F = fun(Ctx) ->
                        UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and rsc_id <> $1 and key = $2", [Id, Username1], Ctx),
                        case UniqueTest of
                            0 ->
                                case z_db:q("
                                        update identity
                                        set key = $2,
                                            modified = now()
                                        where rsc_id = $1
                                          and type = 'username_pw'",
                                        [ Id, Username1 ],
                                        Ctx)
                                of
                                    1 -> ok;
                                    0 -> {error, enoent};
                                    {error, _} ->
                                        {error, eexist} % assume duplicate key error?
                                end;
                            _Other ->
                                {error, eexist}
                        end
                    end,
                    case z_db:transaction(F, Context) of
                        ok ->
                            z:info(
                                "Change of username for user ~p (~s)",
                                [ Id, Username1 ],
                                [ {module, ?MODULE} ],
                                Context),
                            z_mqtt:publish(["~site", "rsc", Id, "identity"], {identity, <<"username_pw">>}, Context),
                            z_depcache:flush(Id, Context),
                            ok;
                        {rollback, {error, _} = Error, _Trace} ->
                            Error;
                        {error, _} = Error ->
                            Error
                    end
            end;
        false ->
            {error, eacces}
    end;
set_username(Id, Username, Context) ->
    set_username( m_rsc:rid(Id, Context), Username, Context ).

%% @doc Set the username/password of a resource.  Replaces any existing username/password.
-spec set_username_pw( m_rsc:resource() | undefined, string(), string(), z:context()) -> ok | {error, Reason :: term()}.
set_username_pw(undefined, _, _, _) ->
    {error, enoent};
set_username_pw(1, _, _, _) ->
    throw({error, admin_password_cannot_be_set});
set_username_pw(Id, Username, Password, Context) when is_integer(Id) ->
    case is_allowed_set_username(Id, Context) of
        true ->
            Username1 = z_string:trim(z_string:to_lower(Username)),
            IsForceDifferent = z_convert:to_bool( m_config:get_value(site, password_force_different, Context) ),
            set_username_pw_1(IsForceDifferent, m_rsc:rid(Id, Context), Username1, Password, Context);
        false ->
            {error, eacces}
    end;
set_username_pw(Id, Username, Password, Context) ->
    set_username_pw( m_rsc:rid(Id, Context), Username, Password, Context ).

set_username_pw_1(true, Id, Username, Password, Context) ->
    case check_username_pw_1(Username, Password, Context) of
        {ok, _} ->
            {error, password_match};
        {error, E} when E =:= nouser; E =:= password ->
            set_username_pw_2(Id, Username, Password, Context);
        {error, _} = Error ->
            Error
    end;
set_username_pw_1(false, Id, Username, Password, Context) when is_integer(Id) ->
    set_username_pw_2(Id, Username, Password, Context).


set_username_pw_2(Id, Username, Password, Context) when is_integer(Id) ->
    Hash = hash(Password),
    case z_db:transaction(fun(Ctx) -> set_username_pw_trans(Id, Username, Hash, Ctx) end, Context) of
        {ok, S} ->
            case S of
                new ->
                    z:info(
                        "New username/password for user ~p (~s)",
                        [ Id, Username ],
                        [ {module, ?MODULE} ],
                        Context);
                exists ->
                    z:info(
                        "Change of username/password for user ~p (~s)",
                        [ Id, Username ],
                        [ {module, ?MODULE} ],
                        Context)
            end,
            reset_rememberme_token(Id, Context),
            z_mqtt:publish(["~site", "rsc", Id, "identity"], {identity, <<"username_pw">>}, Context),
            z_depcache:flush(Id, Context),
            z_session_manager:stop_other_sessions(Id, Context),
            ok;
        {rollback, {{error, _} = Error, _Trace} = ErrTrace} ->
            lager:error("set_username_pw error for ~p, setting username. ~p: ~p",
                        [Username, Error, ErrTrace]),
            Error;
        {error, _} = Error ->
            lager:error("set_username_pw error for ~p, setting username. ~p",
                        [Username, Error]),
            Error
    end.

set_username_pw_trans(Id, Username, Hash, Context) ->
    case z_db:q("
                update identity
                set key = $2,
                    propb = $3,
                    is_verified = true,
                    modified = now()
                where type = 'username_pw'
                  and rsc_id = $1",
                [Id, Username, ?DB_PROPS(Hash)],
                Context)
    of
        0 ->
            case is_reserved_name(Username) of
                true ->
                    {rollback, {error, eexist}};
                false ->
                    UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and key = $1",
                                         [Username],
                                         Context),
                    case UniqueTest of
                        0 ->
                            1 = z_db:q("insert into identity (rsc_id, is_unique, is_verified, type, key, propb)
                                        values ($1, true, true, 'username_pw', $2, $3)",
                                        [ Id, Username, ?DB_PROPS(Hash) ],
                                        Context),
                            z_db:q("update rsc set creator_id = id where id = $1 and creator_id <> id", [Id], Context),
                            {ok, new};
                        _Other ->
                            {rollback, {error, eexist}}
                    end
            end;
        1 ->
            {ok, exists}
    end.

%% @doc Ensure that the user has an associated username and password
%% @spec ensure_username_pw(RscId, Context) -> ok | {error, Reason}
-spec ensure_username_pw(m_rsc:resource(), #context{}) -> ok | {error, term()}.
ensure_username_pw(1, _Context) ->
    throw({error, admin_password_cannot_be_set});
ensure_username_pw(Id, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) orelse z_acl:user(Context) == Id of
        true ->
            RscId = m_rsc:rid(Id, Context),
            case z_db:q1("select count(*) from identity where type = 'username_pw' and rsc_id = $1", [RscId], Context) of
                0 ->
                    Username = generate_username(RscId, Context),
                    Password = z_ids:id(),
                    set_username_pw(RscId, Username, Password, Context);
                _N ->
                    ok
            end;
        false ->
            {error, eacces}
    end.

generate_username(Id, Context) ->
    Username = base_username(Id, Context),
    username_unique(Username, Context).

username_unique(U, Context) ->
    case z_db:q1("select count(*) from identity where type = 'username_pw' and key = $1", [U], Context) of
        0 -> U;
        _ -> username_unique_x(U, 10, Context)
    end.

username_unique_x(U, X, Context) ->
    N = z_convert:to_binary(z_ids:number(X)),
    U1 = <<U/binary, $., N/binary>>,
    case z_db:q1("select count(*) from identity where type = 'username_pw' and key = $1", [U1], Context) of
        0 -> U1;
        _ -> username_unique_x(U, X*10, Context)
    end.


base_username(Id, Context) ->
    T1 = iolist_to_binary([
                z_convert:to_binary(m_rsc:p_no_acl(Id, name_first, Context)),
                " ",
                z_convert:to_binary(m_rsc:p_no_acl(Id, name_surname, Context))
            ]),
    case nospace(z_string:trim(T1)) of
        <<>> ->
            case nospace(m_rsc:p_no_acl(Id, title, Context)) of
                <<>> -> z_convert:to_binary(z_ids:identifier(6));
                Title -> Title
            end;
        Name ->
            Name
    end.

nospace(undefined) ->
    <<>>;
nospace([]) ->
    <<>>;
nospace(<<>>) ->
    <<>>;
nospace(S) ->
    S1 = z_string:truncate(z_string:trim(S), 32, ""),
    S2 = z_string:to_slug(S1),
    nodash(binary:replace(S2, <<"-">>, <<".">>, [global])).

nodash(<<".">>) ->
    <<>>;
nodash(S) ->
    case binary:replace(S, <<"..">>, <<".">>, [global]) of
        S -> S;
        S1 -> nodash(S1)
    end.


%% @doc Return the rsc_id with the given username/password.
%%      If succesful then updates the 'visited' timestamp of the entry.
-spec check_username_pw(binary() | string(), binary() | string(), z:context()) ->
            {ok, m_rsc:resource_id()} | {error, term()}.
check_username_pw(Username, Password, Context) ->
    check_username_pw(Username, Password, [], Context).


%% @doc Return the rsc_id with the given username/password.
%% If succesful then updates the 'visited' timestamp of the entry.
%% Use a timer to level the time difference between existing and non existing accounts.
-spec check_username_pw(binary() | string(), binary() | string(), list(), z:context()) ->
            {ok, m_rsc:resource_id()} | {error, term()}.
check_username_pw(Username, Password, QueryArgs, Context) ->
    Timeout = ?DEFAULT_PW_CHECK_DURATION + z_ids:number(?DEFAULT_PW_CHECK_VARIANCE),
    Ref = erlang:make_ref(),
    erlang:send_after(Timeout, self(), {pw_done, Ref}),
    Result = check_username_pw_do(Username, Password, QueryArgs, Context),
    wait_message(Ref),
    Result.

wait_message(Ref) ->
    receive
        {pw_done, Ref} ->
            ok;
        {pw_done, _} ->
            wait_message(Ref)
    after ?DEFAULT_PW_CHECK_DURATION + ?DEFAULT_PW_CHECK_DURATION ->
        lager:error("Timeout waiting for pw_done message."),
        ok
    end.

check_username_pw_do(Username, Password, QueryArgs, Context) ->
    NormalizedUsername = z_convert:to_binary( z_string:trim( z_string:to_lower(Username) ) ),
    case z_notifier:first(#auth_precheck{ username =  NormalizedUsername }, Context) of
        Ok when Ok =:= ok; Ok =:= undefined ->
            case post_check( check_username_pw_1(NormalizedUsername, Password, Context), QueryArgs, Context ) of
                {ok, RscId} ->
                    z_notifier:notify_sync(
                        #auth_checked{
                            id = RscId,
                            username = NormalizedUsername,
                            is_accepted = true
                        },
                        Context),
                    {ok, RscId};
                {error, need_passcode} = Error ->
                    Error;
                Error ->
                    z_notifier:notify_sync(
                        #auth_checked{
                            id = undefined,
                            username = NormalizedUsername,
                            is_accepted = false
                        },
                        Context),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

post_check({ok, RscId}, QueryArgs, Context) ->
    case z_notifier:first(#auth_postcheck{ id = RscId, query_args = QueryArgs }, Context) of
        ok -> {ok, RscId};
        undefined -> {ok, RscId};
        Error -> Error
    end;
post_check(Error, _QueryArgs, _Context) ->
    Error.

check_username_pw_1(<<"admin">>, "", _Context) ->
    {error, password};
check_username_pw_1(<<"admin">>, <<>>, _Context) ->
    {error, password};
check_username_pw_1(<<"admin">>, Password, Context) ->
    Password1 = z_convert:to_list(Password),
    case z_convert:to_list(m_site:get(admin_password, Context)) of
        "admin" when Password1 =:= "admin" ->
            % Only allow default password from whitelisted ip addresses
            case is_peer_whitelisted(Context) of
                true ->
                    z_db:q("update identity set visited = now() where id = 1", Context),
                    {ok, 1};
                false ->
                    lager:error(
                        "admin login with default password from non whitelisted ip address ~p",
                        [m_req:get(peer, Context)]
                    ),
                    {error, peer_not_whitelisted}
            end;
        Password1 ->
            z_db:q("update identity set visited = now() where id = 1", Context),
            {ok, 1};
        _ ->
            {error, password}
    end;
check_username_pw_1(Username, Password, Context) ->
    Username1 = z_string:trim(z_string:to_lower(Username)),
    Row = z_db:q_row("select rsc_id, propb from identity where type = 'username_pw' and key = $1", [Username1], Context),
    case Row of
        undefined ->
            % If the Username looks like an e-mail address, try by Email & Password
            case z_email_utils:is_email(Username1) of
                true -> check_email_pw(Username1, Password, Context);
                false -> {error, nouser}
            end;
        {RscId, Hash} ->
            check_hash(RscId, Username, Password, Hash, Context)
    end.

%% @doc Check if the tcp/ip peer address is a whitelisted ip address
is_peer_whitelisted(Context) ->
    Peer = z_convert:to_list(m_req:get(peer, Context)),
    is_ip_whitelisted(Peer, Context).

is_ip_whitelisted(IP, Context) ->
    Whitelist = string:tokens(z_convert:to_list(ip_whitelist(Context)), ","),
    ip_match(IP, Whitelist).

ip_match(_Peer, []) ->
    false;
ip_match(Peer, [Peer|_IPs]) ->
    true;
ip_match(Peer, [IP|IPs]) ->
    case string:tokens(IP, "/") of
        [IPVal,Bits] ->
            {ok, MatchAdr} = inet:parse_address(IPVal),
            {ok, PeerAdr} = inet:parse_address(Peer),
            BitsNr = z_convert:to_integer(Bits),
            case ip_match_mask(PeerAdr, MatchAdr, BitsNr) of
                true -> true;
                false -> ip_match(Peer, IPs)
            end;
        _ ->
            ip_match(Peer, IPs)
    end.

ip_match_mask({_,_,_,_} = Peer, {_,_,_,_} = Match, Bits) ->
    ip_match_mask_1(Peer, Match, 32-Bits);
ip_match_mask({_,_,_,_,_,_,_,_} = Peer, {_,_,_,_,_,_,_,_} = Match, Bits) ->
    ip_match_mask_1(Peer, Match, 128-Bits);
ip_match_mask(_, _, _) ->
    false.

ip_match_mask_1(PeerAdr, MatchAdr, MaskBits) ->
    NetMask = ((1 bsl 128) - 1) bsl MaskBits,
    (to_ip_number(MatchAdr) band NetMask) =:= (to_ip_number(PeerAdr) band NetMask).

to_ip_number({A,B,C,D}) ->
    ((((A * 256) + B) * 256) + C) * 256 + D;
to_ip_number({A,B,C,D,E,F,G,H}) ->
    (((((((((A * 65536) + B) * 65536) + C) * 65536 + D) * 65536 + E) * 65536 + F) * 65536) + G) * 65536 + H.

ip_whitelist(Context) ->
    SiteWhitelist = m_config:get_value(site, ip_whitelist, Context),
    case z_utils:is_empty(SiteWhitelist) of
        true ->
            z_config:get(ip_whitelist);
        false ->
            SiteWhitelist
    end.

%% @doc Check is the password belongs to an user with the given e-mail address.
%%      Multiple users can have the same e-mail address, so multiple checks are needed.
%%      If succesful then updates the 'visited' timestamp of the entry.
%% @spec check_email_pw(Email, Password, Context) -> {ok, Id} | {error, Reason}
check_email_pw(Email, Password, Context) ->
    case lookup_by_type_and_key_multi(email, Email, Context) of
        [] -> {error, nouser};
        Users -> check_email_pw_1(Users, Email, Password, Context)
    end.

check_email_pw_1([], _Email, _Password, _Context) ->
    {error, password};
check_email_pw_1([Idn|Rest], Email, Password, Context) ->
    UserId = proplists:get_value(rsc_id, Idn),
    Row = z_db:q_row("select rsc_id, key, propb from identity where type = 'username_pw' and rsc_id = $1",
                     [UserId], Context),
    case Row of
        undefined ->
            check_email_pw_1(Rest, Email, Password, Context);
        {RscId, Username, Hash} ->
            case check_hash(RscId, Username, Password, Hash, Context) of
                {ok, Id} -> {ok, Id};
                {error, password} ->
                    check_email_pw_1(Rest, Email, Password, Context)
            end
    end.

%% @doc Find the user id connected to the 'rememberme' cookie value.
-spec lookup_by_rememberme_token(binary(), #context{}) -> {ok, pos_integer()} | {error, enoent}.
lookup_by_rememberme_token(Token, Context) ->
    case z_db:q1("select rsc_id from identity where key = $1", [Token], Context) of
        undefined ->
            {error, enoent};
        Id when is_integer(Id) ->
            {ok, Id}
    end.

%% @doc Find the 'rememberme' cookie value for the user, generate a new one if not found.
-spec get_rememberme_token(m_rsc:resource(), #context{}) -> {ok, binary()}.
get_rememberme_token(UserId, Context) ->
    case z_db:q1("select key from identity
                  where type = 'rememberme'
                    and rsc_id = $1", [m_rsc:rid(UserId, Context)], Context) of
        undefined ->
            reset_rememberme_token(UserId, Context);
        Token ->
            {ok, Token}
    end.

%% @doc Reset the 'rememberme' cookie value. Needed if an user's password is changed.
-spec reset_rememberme_token(m_rsc:resource(), #context{}) -> {ok, binary()}.
reset_rememberme_token(UserId, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            delete_by_type(UserId, rememberme, Ctx),
            Token = new_unique_key(rememberme, Ctx),
            {ok, _} = insert_unique(UserId, rememberme, Token, Ctx),
            {ok, Token}
        end,
        Context).

new_unique_key(Type, Context) ->
    Key = z_convert:to_binary(z_ids:id()),
    case z_db:q1("select id
                  from identity
                  where type = $1
                    and key = $2",
                 [Type, Key],
                 Context)
    of
        undefined ->
            Key;
        _Id ->
            new_unique_key(Type, Context)
    end.


%% @doc Fetch a specific identity entry.
get(IdnId, Context) ->
    z_db:assoc_row("select * from identity where id = $1", [IdnId], Context).

%% @doc Fetch all credentials belonging to the user "id"
-spec get_rsc(m_rsc:resource(), #context{}) -> list().
get_rsc(Id, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1", [m_rsc:rid(Id, Context)], Context).


%% @doc Fetch all different identity types of an user
-spec get_rsc_types(m_rsc:resource(), #context{}) -> list().
get_rsc_types(Id, Context) ->
    Rs = z_db:q("select type from identity where rsc_id = $1", [m_rsc:rid(Id, Context)], Context),
    [ R || {R} <- Rs ].

%% @doc Fetch all credentials belonging to the user "id" and of a certain type
-spec get_rsc_by_type(m_rsc:resource(), atom(), #context{}) -> list().
get_rsc_by_type(Id, email, Context) ->
    Idns = get_rsc_by_type_1(Id, email, Context),
    case normalize_key(email, m_rsc:p_no_acl(Id, email, Context)) of
        undefined ->
            Idns;
        Email ->
            IsMissing = is_valid_key(email, Email, Context)
                        andalso not lists:any(fun(Idn) ->
                                                 proplists:get_value(key, Idn) =:= Email
                                              end,
                                              Idns),
            case IsMissing of
                true ->
                    insert(Id, email, Email, Context),
                    get_rsc_by_type_1(Id, email, Context);
                false ->
                    Idns
            end
    end;
get_rsc_by_type(Id, Type, Context) ->
    get_rsc_by_type_1(Id, Type, Context).

get_rsc_by_type_1(Id, Type, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1 and type = $2 order by is_verified desc, key asc",
               [m_rsc:rid(Id, Context), Type], Context).

-spec get_rsc(m_rsc:resource(), atom(), #context{}) -> list() | undefined.
get_rsc(Id, Type, Context) ->
    z_db:assoc_row("select * from identity where rsc_id = $1 and type = $2", [m_rsc:rid(Id, Context), Type], Context).


%% @doc Hash a password, using bcrypt
-spec hash(password()) -> bcrypt_hash().
hash(Pw) ->
    {bcrypt, erlpass:hash(Pw)}.

%% @doc Compare if a password is the same as a hash.
-spec hash_is_equal(password(), hash()) -> boolean().
hash_is_equal(Pw, {bcrypt, Hash}) ->
    erlpass:match(Pw, Hash);
hash_is_equal(Pw, {hash, Salt, Hash}) ->
    NewHash = crypto:hash(sha, [Salt, Pw]),
    Hash =:= NewHash;
hash_is_equal(_, _) ->
    false.


%% @doc Check if the password hash needs to be rehashed.
-spec needs_rehash(hash()) -> boolean().
needs_rehash({bcrypt, _}) ->
    false;
needs_rehash({hash, _, _}) ->
    true.


%% @doc Create an identity record.
-spec insert(m_rsc:resource(), atom(), binary(), #context{}) -> {ok, pos_integer()} | {error, invalid_key}.
insert(Rsc, Type, Key, Context) ->
    insert(Rsc, Type, Key, [], Context).
insert(Rsc, Type, Key, Props, Context) ->
    KeyNorm = normalize_key(Type, Key),
    case is_valid_key(Type, KeyNorm, Context) of
        true -> insert_1(Rsc, Type, KeyNorm, Props, Context);
        false -> {error, invalid_key}
    end.

insert_1(Rsc, Type, Key, Props, Context) ->
    RscId = m_rsc:rid(Rsc, Context),
    case z_db:q1("select id
                  from identity
                  where rsc_id = $1
                    and type = $2
                    and key = $3",
                [RscId, Type, Key],
                Context)
    of
        undefined ->
            Props1 = [{rsc_id, RscId}, {type, Type}, {key, Key} | Props],
            Result = z_db:insert(identity, validate_is_unique(Props1), Context),
            z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context),
            Result;
        IdnId ->
            case proplists:get_value(is_verified, Props, false) of
                true ->
                    set_verified_trans(RscId, Type, Key, Context),
                    z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context);
                false ->
                    nop
            end,
            {ok, IdnId}
    end.

% The is_unique flag is 'null' if the entry is not unique.
% This enables using an 'unique' constraint.
validate_is_unique(Props) ->
    case proplists:get_value(is_unique, Props) of
        false ->
            [{is_unique, undefined} | proplists:delete(is_unique, Props)];
        _ ->
            Props
    end.

is_valid_key(_Type, undefined, _Context) ->
    false;
is_valid_key(email, Key, _Context) ->
    z_email_utils:is_email(Key);
is_valid_key(username_pw, Key, _Context) ->
    not is_reserved_name(Key);
is_valid_key(_Type, _Key, _Context) ->
    true.

normalize_key(_Type, undefined) ->
    undefined;
normalize_key(email, Key) ->
    z_convert:to_binary(z_string:trim(z_string:to_lower(Key)));
normalize_key(username_pw, Key) ->
    z_convert:to_binary(z_string:trim(z_string:to_lower(Key)));
normalize_key(_Type, Key) ->
    Key.


%% @doc Create an unique identity record.
insert_unique(RscId, Type, Key, Context) ->
    insert(RscId, Type, Key, [{is_unique, true}], Context).
insert_unique(RscId, Type, Key, Props, Context) ->
    insert(RscId, Type, Key, [{is_unique, true}|Props], Context).


%% @doc Set the visited timestamp for the given user.
%% @todo Make this a log - so that we can see the last visits and check if this is from a new browser/ip address.
set_visited(UserId, Context) ->
    z_db:q("update identity set visited = now() where rsc_id = $1 and type = 'username_pw'", [m_rsc:rid(UserId, Context)], Context).


%% @doc Set the verified flag on a record by identity id.
-spec set_verified(integer(), #context{}) -> ok | {error, notfound}.
set_verified(Id, Context) ->
    case z_db:q_row("select rsc_id, type from identity where id = $1", [Id], Context) of
        {RscId, Type} ->
            case z_db:q("
                    update identity
                    set is_verified = true,
                        verify_key = null,
                        modified = now()
                    where id = $1",
                    [Id],
                    Context)
            of
                1 ->
                    z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context),
                    ok;
                0 ->
                    {error, notfound}
            end;
        undefined ->
            {error, notfound}
    end.


%% @doc Set the verified flag on a record by rescource id, identity type and value (eg an user's email address).
set_verified(RscId, Type, Key, Context)
    when is_integer(RscId),
         Type =/= undefined,
         Key =/= undefined, Key =/= <<>>, Key =/= [] ->
    Result = z_db:transaction(fun(Ctx) -> set_verified_trans(RscId, Type, Key, Ctx) end, Context),
    z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context),
    Result;
set_verified(_RscId, _Type, _Key, _Context) ->
    {error, badarg}.

set_verified_trans(RscId, Type, Key, Context) ->
    case z_db:q("update identity
                 set is_verified = true,
                     verify_key = null,
                     modified = now()
                 where rsc_id = $1
                   and type = $2
                   and key = $3",
                [RscId, Type, Key],
                Context)
    of
        0 ->
            1 = z_db:q("insert into identity (rsc_id, type, key, is_verified)
                        values ($1,$2,$3,true)",
                       [RscId, Type, Key],
                       Context),
            ok;
        N when N > 0 ->
            ok
    end.

%% @doc Check if there is a verified identity for the user, beyond the username_pw
is_verified(RscId, Context) ->
    case z_db:q1("select id from identity where rsc_id = $1 and is_verified = true and type <> 'username_pw'",
                [RscId], Context) of
        undefined -> false;
        _ -> true
    end.

-spec set_by_type(m_rsc:resource(), string(), string(), #context{}) -> ok.
set_by_type(RscId, Type, Key, Context) ->
    set_by_type(RscId, Type, Key, [], Context).
set_by_type(RscId, Type, Key, Props, Context) ->
	F = fun(Ctx) ->
		case z_db:q("
                update identity
                set key = $3,
                    propb = $4,
                    modified = now()
                where rsc_id = $1
                  and type = $2",
                [ m_rsc:rid(RscId, Context), Type, Key, ?DB_PROPS(Props) ],
                Ctx)
        of
			0 ->
                z_db:q("insert into identity (rsc_id, type, key, propb) values ($1,$2,$3,$4)",
                       [ m_rsc:rid(RscId, Context), Type, Key, ?DB_PROPS(Props) ],
                       Ctx);
            N when N > 0 ->
                ok
		end,
        ok
	end,
	z_db:transaction(F, Context).

delete(IdnId, Context) ->
    case z_db:q_row("select rsc_id, type, key from identity where id = $1", [IdnId], Context) of
        undefined ->
            {ok, 0};
        {RscId, Type, Key} ->
            case z_acl:rsc_editable(RscId, Context) of
                true ->
                    case z_db:delete(identity, IdnId, Context) of
                        {ok, 1} ->
                            z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context),
                            maybe_reset_email_property(RscId, Type, Key, Context),
                            {ok, 1};
                        Other ->
                            Other
                    end;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Move the identities of two resources, the identities are removed from the source id.
-spec merge(m_rsc:resource(), m_rsc:resource(), #context{}) -> ok | {error, term()}.
merge(WinnerId, LooserId, Context) ->
    case z_acl:rsc_editable(WinnerId, Context) andalso z_acl:rsc_editable(LooserId, Context) of
        true ->
            F = fun(Ctx) ->
                % Move all identities to the winner, except for duplicate type+key combinations
                LooserIdns = z_db:q("select type, key, id from identity where rsc_id = $1", [m_rsc:rid(LooserId, Context)], Ctx),
                WinIdns = z_db:q("select type, key from identity where rsc_id = $1", [m_rsc:rid(WinnerId, Context)], Ctx),
                AddIdns = lists:filter(
                                fun({Type, Key, _Id}) ->
                                    case is_unique_identity_type(Type) of
                                        true ->
                                            not proplists:is_defined(Type, WinIdns);
                                        false ->
                                            not lists:member({Type, Key}, WinIdns)
                                    end
                                end,
                                LooserIdns),
                lists:foreach(
                            fun({_Type, _Key, Id}) ->
                                z_db:q("update identity set rsc_id = $1 where id = $2",
                                       [m_rsc:rid(WinnerId, Context), Id],
                                       Ctx)
                            end,
                            AddIdns),
                case proplists:is_defined(<<"username_pw">>, AddIdns) of
                    true ->
                        z_db:q("update rsc set creator_id = id where id = $1 and creator_id <> id", [m_rsc:rid(WinnerId, Context)], Context);
                    false ->
                        ok
                end
            end,
            z_db:transaction(F, Context),
            z_mqtt:publish(["~site", "rsc", m_rsc:rid(LooserId, Context), "identity"], {identity, all}, Context),
            z_mqtt:publish(["~site", "rsc", m_rsc:rid(WinnerId, Context), "identity"], {identity, all}, Context),
            ok;
        false ->
            {error, eacces}
    end.

is_unique_identity_type(<<"username_pw">>) -> true;
is_unique_identity_type(_) -> false.


%% @doc If an email identity is deleted, then ensure that the 'email' property is reset accordingly.
maybe_reset_email_property(Id, <<"email">>, Email, Context) when is_binary(Email) ->
    case normalize_key(email, m_rsc:p_no_acl(Id, email_raw, Context)) of
        Email ->
            NewEmail = z_db:q1("
                    select key
                    from identity
                    where rsc_id = $1
                      and type = 'email'
                    order by is_verified desc, modified desc",
                    [Id],
                    Context),
            Context1 = z_context:set(is_m_identity_update, true, Context),
            {ok, _} = m_rsc:update(Id, [{email, NewEmail}], Context1),
            ok;
        _ ->
            ok
    end;
maybe_reset_email_property(_Id, _Type, _Key, _Context) ->
    ok.


-spec delete_by_type(m_rsc:resource(), atom(), #context{}) -> ok.
delete_by_type(RscId, Type, Context) ->
	case z_db:q("delete from identity where rsc_id = $1 and type = $2", [m_rsc:rid(RscId, Context), Type], Context) of
        0 -> ok;
        _N -> z_mqtt:publish(["~site", "rsc", m_rsc:rid(RscId, Context), "identity"], {identity, Type}, Context)
    end.

-spec delete_by_type_and_key(m_rsc:resource(), atom(), atom(), #context{}) -> ok.
delete_by_type_and_key(RscId, Type, Key, Context) ->
    case z_db:q("delete from identity where rsc_id = $1 and type = $2 and key = $3", [m_rsc:rid(RscId, Context), Type, Key], Context) of
        0 -> ok;
        _N -> z_mqtt:publish(["~site", "rsc", RscId, "identity"], {identity, Type}, Context)
    end.

lookup_by_username(Key, Context) ->
	lookup_by_type_and_key("username_pw", z_string:to_lower(Key), Context).

lookup_by_type_and_key(Type, Key, Context) ->
    z_db:assoc_row("select * from identity where type = $1 and key = $2", [Type, Key], Context).

lookup_by_type_and_key_multi(Type, Key, Context) ->
    z_db:assoc("select * from identity where type = $1 and key = $2", [Type, Key], Context).

lookup_by_verify_key(Key, Context) ->
    z_db:assoc_row("select * from identity where verify_key = $1", [Key], Context).

set_verify_key(Id, Context) ->
    N = z_ids:id(10),
    case lookup_by_verify_key(N, Context) of
        undefined ->
            z_db:q("update identity
                    set verify_key = $2,
                        modified = now()
                    where id = $1",
                    [Id, N],
                    Context),
            {ok, N};
        _ ->
            set_verify_key(Id, Context)
    end.


check_hash(RscId, Username, Password, Hash, Context) ->
    N = #identity_password_match{rsc_id=RscId, password=Password, hash=Hash},
    case z_notifier:first(N, Context) of
        {ok, rehash} ->
            %% OK but module says it needs rehashing; do that using
            %% the current hashing mechanism
            ok = set_username_pw(RscId, Username, Password, z_acl:sudo(Context)),
            check_hash_ok(RscId, Context);
        ok ->
            check_hash_ok(RscId, Context);
        {error, Reason} ->
            {error, Reason};
        undefined ->
            {error, nouser}
    end.


check_hash_ok(RscId, _Context) ->
    {ok, RscId}.

%% @doc Prevent insert of reserved usernames.
%% See: http://tools.ietf.org/html/rfc2142
%% See: http://arstechnica.com/security/2015/03/bogus-ssl-certificate-for-windows-live-could-allow-man-in-the-middle-hacks/
is_reserved_name(List) when is_list(List) ->
    is_reserved_name(z_convert:to_binary(List));
is_reserved_name(Name) when is_binary(Name) ->
    is_reserved_name_1(z_string:trim(z_string:to_lower(Name))).

is_reserved_name_1(<<>>) -> true;
is_reserved_name_1(<<"admin">>) -> true;
is_reserved_name_1(<<"administrator">>) -> true;
is_reserved_name_1(<<"postmaster">>) -> true;
is_reserved_name_1(<<"hostmaster">>) -> true;
is_reserved_name_1(<<"webmaster">>) -> true;
is_reserved_name_1(<<"abuse">>) -> true;
is_reserved_name_1(<<"security">>) -> true;
is_reserved_name_1(<<"root">>) -> true;
is_reserved_name_1(<<"www">>) -> true;
is_reserved_name_1(<<"uucp">>) -> true;
is_reserved_name_1(<<"ftp">>) -> true;
is_reserved_name_1(<<"usenet">>) -> true;
is_reserved_name_1(<<"news">>) -> true;
is_reserved_name_1(<<"wwwadmin">>) -> true;
is_reserved_name_1(<<"webadmin">>) -> true;
is_reserved_name_1(<<"mail">>) -> true;
is_reserved_name_1(_) -> false.


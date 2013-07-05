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

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    is_user/2,
    get_username/1,
    get_username/2,
    delete_username/2,
    set_username/3,
    set_username_pw/4,
    check_username_pw/3,
    hash/1,
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

	set_by_type/4,
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
    
    delete/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined} = M, _Context) ->
    M#m{value=Id};
m_find_value(is_user, #m{value=RscId}, Context) ->
    is_user(RscId, Context);
m_find_value(username, #m{value=RscId}, Context) ->
    get_username(RscId, Context);
m_find_value(all, #m{value=RscId} = M, _Context) ->
    M#m{value={all, RscId}};
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
is_user(Id, Context) ->
    case z_db:q1("select count(*) from identity where rsc_id = $1 and type in ('username_pw', 'openid')", [Id], Context) of
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
get_username(Id, Context) ->
    z_db:q1("select key from identity where rsc_id = $1 and type = 'username_pw'", [Id], Context).


%% @doc Delete an username from a resource.
%% @spec delete_username(ResourceId, Context) -> void
delete_username(Id, Context) when is_integer(Id), Id /= 1 ->
    case z_acl:is_allowed(delete, Id, Context) orelse z_acl:user(Context) == Id of
        true ->  z_db:q("delete from identity where rsc_id = $1 and type = 'username_pw'", [Id], Context);
        false -> {error, eacces}
    end.


%% @doc Change the username of the resource id, only possible if there is already an username/password set
%% @spec set_username(ResourceId, Username, Context) -> ok | {error, Reason}
set_username(Id, Username, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) orelse z_acl:user(Context) == Id of
        true ->
            Username1 = z_string:to_lower(Username),
            F = fun(Ctx) ->
                UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and rsc_id <> $1 and key = $2", [Id, Username1], Ctx),
                case UniqueTest of
                    0 ->
                        case z_db:q("update identity set key = $2 where rsc_id = $1 and type = 'username_pw'", [Id, Username1], Ctx) of
                            1 -> ok;
                            0 -> {error, enoent};
                            {error, _} -> {error, eexist} % assume duplicate key error?
                        end;
                    _Other ->
                        {error, eexist}
                end
            end,
            z_db:transaction(F, Context);
        false ->
            {error, eacces}
    end.


%% @doc Set the username/password of a resource.  Replaces any existing username/password.
%% @spec set_username_pw(RscId, Username, Password, Context) -> ok | {error, Reason}
set_username_pw(1, _, _, _) ->
    throw({error, admin_password_cannot_be_set});

set_username_pw(Id, Username, Password, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) orelse z_acl:user(Context) == Id of
        true ->
            Username1 = z_string:to_lower(Username),
            Hash = hash(Password),
            F = fun(Ctx) ->
                Rupd = z_db:q("
                            update identity 
                            set key = $2,
                                propb = $3,
                                is_verified = true,
                                modified = now()
                            where type = 'username_pw' and rsc_id = $1", [Id, Username1, Hash], Ctx),
                case Rupd of
                    0 ->
                        UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and key = $1", [Username1], Ctx),
                        case UniqueTest of
                            0 ->
                                Rows = z_db:q("insert into identity (rsc_id, is_unique, is_verified, type, key, propb) values ($1, true, true, 'username_pw', $2, $3)", [Id, Username1, Hash], Ctx),
                                z_db:q("update rsc set creator_id = id where id = $1 and creator_id <> id", [Id], Ctx),
                                Rows;
                            _Other ->
                                throw({error, eexist})
                        end;
                    1 -> 
                        1
                end
            end,
            case z_db:transaction(F, Context) of
                1 ->
                    z_depcache:flush(Id, Context),
                    ok;
                R ->
                    R
            end;
        false ->
            {error, eacces}
    end.


%% @doc Return the rsc_id with the given username/password.  
%%      If succesful then updates the 'visited' timestamp of the entry.
%% @spec check_username_pw(Username, Password, Context) -> {ok, Id} | {error, Reason}
check_username_pw("admin", [], _Context) ->
    {error, password};
check_username_pw("admin", Password, Context) ->
    Password1 = z_convert:to_list(Password),
    case m_site:get(admin_password, Context) of
	Password1 ->
	    z_db:q("update identity set visited = now() where id = 1", Context),
	    {ok, 1};
	_ ->
	    {error, password}
    end;
check_username_pw(Username, Password, Context) ->
    Username1 = z_string:trim(z_string:to_lower(Username)),
    Row = z_db:q_row("select rsc_id, propb from identity where type = 'username_pw' and key = $1", [Username1], Context),
    case Row of
        undefined ->
            % If the Username looks like an e-mail address, try by Email & Password
            case z_email_utils:is_email(Username) of
                true -> check_email_pw(Username, Password, Context);
                false -> {error, nouser}
            end;
        {RscId, Hash} ->
            case hash_is_equal(Password, Hash) of
                true -> 
                    set_visited(RscId, Context),
                    {ok, RscId};
                false ->
                    {error, password}
            end
    end.

%% @doc Check is the password belongs to an user with the given e-mail address.
%%      Multiple users can have the same e-mail address, so multiple checks are needed.
%%      If succesful then updates the 'visited' timestamp of the entry.
%% @spec check_email_pw(Email, Password, Context) -> {ok, Id} | {error, Reason}
check_email_pw(Email, Password, Context) ->
    EmailLower = z_string:trim(z_string:to_lower(Email)),
    case lookup_by_type_and_key_multi(email, EmailLower, Context) of
        [] -> {error, nouser};
        Users -> check_email_pw_1(Users, Password, Context)
    end.

check_email_pw_1([], _Password, _Context) ->
    {error, password};
check_email_pw_1([Idn|Rest], Password, Context) ->
    UserId = proplists:get_value(rsc_id, Idn),
    Row = z_db:q_row("select rsc_id, propb from identity where type = 'username_pw' and rsc_id = $1", 
                     [UserId], Context),
    case Row of
        undefined -> 
            check_email_pw_1(Rest, Password, Context);
        {RscId, Hash} ->
            case hash_is_equal(Password, Hash) of
                true -> 
                    set_visited(RscId, Context),
                    {ok, RscId};
                false ->
                    check_email_pw_1(Rest, Password, Context)
            end
    end.


%% @doc Fetch a specific identity entry.
get(IdnId, Context) ->
    z_db:assoc_row("select * from identity where id = $1", [IdnId], Context).

%% @doc Fetch all credentials belonging to the user "id"
%% @spec get_rsc(integer(), context()) -> list()
get_rsc(Id, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1", [Id], Context).

%% @doc Fetch all credentials belonging to the user "id" and of a certain type
get_rsc_by_type(Id, Type, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1 and type = $2 order by is_verified desc, key asc",
               [Id, Type], Context).

get_rsc(Id, Type, Context) ->
    z_db:assoc_row("select * from identity where rsc_id = $1 and type = $2", [Id, Type], Context).


%% @doc Hash a password, using sha1 and a salt
%% @spec hash(Password) -> tuple()
hash(Pw) ->
    Salt = z_ids:id(10),
    Hash = crypto:sha([Salt,Pw]),
    {hash, Salt, Hash}.


%% @doc Compare if a password is the same as a hash.
%% @spec hash_is_equal(Password, Hash) -> bool()
hash_is_equal(Pw, {hash, Salt, Hash}) ->
    NewHash = crypto:sha([Salt, Pw]),
    Hash =:= NewHash;
hash_is_equal(_, _) ->
    false.

%% @doc Create an identity record.
insert(RscId, Type, Key, Context) ->
    insert(RscId, Type, Key, [], Context).
insert(RscId, Type, Key, Props, Context) ->
    KeyNorm = normalize_key(Type, Key),
    case is_valid_key(Type, KeyNorm, Context) of
        true -> insert_1(RscId, Type, KeyNorm, Props, Context);
        false -> {error, invalid_key}
    end.

insert_1(RscId, Type, Key, Props, Context) ->
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
            z_db:insert(identity, validate_is_unique(Props1), Context);
        IdnId ->
            case proplists:get_value(is_verified, Props, false) of
                true -> set_verified_trans(RscId, Type, Key, Context);
                false -> nop
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

is_valid_key(email, Key, _Context) ->
    z_email_utils:is_email(Key);
is_valid_key(_Type, _Key, _Context) ->
    true.

normalize_key(email, Key) ->
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
    z_db:q("update identity set visited = now() where rsc_id = $1 and type = 'username_pw'", [UserId], Context).


%% @doc Set the verified flag on a record by identity id.
set_verified(Id, Context) ->
    z_db:q("update identity set is_verified = true, verify_key = null where id = $1", 
           [Id], 
           Context).

%% @doc Set the verified flag on a record by rescource id, identity type and value (eg an user's email address).
set_verified(RscId, Type, Key, Context) 
    when is_integer(RscId), 
         Type =/= undefined, 
         Key =/= undefined, Key =/= <<>>, Key =/= [] ->
    z_db:transaction(fun(Ctx) -> set_verified_trans(RscId, Type, Key, Ctx) end, Context);
set_verified(_RscId, _Type, _Key, _Context) ->
    {error, badarg}.

set_verified_trans(RscId, Type, Key, Context) ->
    case z_db:q("update identity 
                 set is_verified = true, 
                     verify_key = null 
                 where rsc_id = $1 and type = $2 and key = $3", 
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

set_by_type(RscId, Type, Key, Context) ->
	F = fun(Ctx) -> 
		case z_db:q("update identity set key = $3 where rsc_id = $1 and type = $2", [RscId, Type, Key], Ctx) of
			0 -> z_db:q("insert into identity (rsc_id, type, key) values ($1,$2,$3)", [RscId, Type, Key], Ctx);
            N when N > 0 -> ok
		end
	end,
	z_db:transaction(F, Context).

delete(IdnId, Context) ->
    z_db:delete(identity, IdnId, Context).

delete_by_type(RscId, Type, Context) ->
	z_db:q("delete from identity where rsc_id = $1 and type = $2", [RscId, Type], Context).

delete_by_type_and_key(RscId, Type, Key, Context) ->
    z_db:q("delete from identity where rsc_id = $1 and type = $2 and key = $3", [RscId, Type, Key], Context).


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
            z_db:q("update identity set verify_key = $2 where id = $1", [Id, N], Context),
            {ok, N};
        _ ->
            set_verify_key(Id, Context)
    end.


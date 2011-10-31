%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-25
%%
%% @doc Manage identities of users.  An identity can be an username/password, openid, oauth credentials etc.

%% Copyright 2009 Marc Worrell
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
    get_rsc/2,
    get_rsc/3,

	lookup_by_username/2,
	lookup_by_verify_key/2,
    lookup_by_type_and_key/3,
    lookup_by_type_and_key_multi/3,

	set_by_type/4,
	delete_by_type/3,
	
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
m_find_value(all, #m{value=RscId}, Context) ->
    get_rsc(RscId, Context);
m_find_value(Type, #m{value=RscId}, Context) ->
    get_rsc(RscId, Type, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
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


%% @doc Return the rsc_id with the given username/password.  When succesful then updates the 'visited' timestamp of the entry.
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
    Username1 = z_string:to_lower(Username),
    Row = z_db:q_row("select id, rsc_id, propb from identity where type = 'username_pw' and key = $1", [Username1], Context),
    case Row of
        undefined ->
            {error, nouser};
        {Id, RscId, Hash} ->
            case hash_is_equal(Password, Hash) of
                true -> 
                    z_db:q("update identity set visited = now() where id = $1", [Id], Context),
                    {ok, RscId};
                false ->
                    {error, password}
            end
    end.



%% @doc Fetch all credentials belonging to the user "id"
%% @spec get_rsc(integer(), context()) -> list()
get_rsc(Id, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1", [Id], Context).

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
    Props1 = [{rsc_id, RscId}, {type, Type}, {key, Key} | Props],
    z_db:insert(identity, Props1, Context).

%% @doc Create an unique identity record.
insert_unique(RscId, Type, Key, Context) ->
    insert(RscId, Type, Key, [{is_unique, true}], Context).
insert_unique(RscId, Type, Key, Props, Context) ->
    insert(RscId, Type, Key, [{is_unique, true}|Props], Context).

%% @doc Set the verified flag on a record.
set_verified(Id, Context) ->
    z_db:q("update identity set is_verified = true, verify_key = null where id = $1", [Id], Context).
set_verified(RscId, Type, Key, Context) ->
    z_db:q("update identity set is_verified = true, verify_key = null where rsc_id = $1 and type = $2 and key = $3", [RscId, Type, Key], Context).
    
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
			1 -> ok;
			0 -> z_db:q("insert into identity (rsc_id, type, key) values ($1,$2,$3)", [RscId, Type, Key], Ctx)
		end
	end,
	z_db:transaction(F, Context).

delete(IdnId, Context) ->
    z_db:delete(identity, IdnId, Context).

delete_by_type(RscId, Type, Context) ->
	z_db:q("delete from identity where rsc_id = $1 and type = $2", [RscId, Type], Context).


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


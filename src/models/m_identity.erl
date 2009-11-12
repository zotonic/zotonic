%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-25
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
    get_username/2,
    delete_username/2,
    set_username/3,
    set_username_pw/4,
    check_username_pw/3,
    hash/1,
    hash_is_equal/2
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

%% @doc Return the username of the resource id, undefined if no username
%% @spec get_username(ResourceId, Context) -> Username | undefined
get_username(Id, Context) ->
    z_db:q1("select key from identity where rsc_id = $1 and type = 'username_pw'", [Id], Context).


%% @doc Delete an username from a resource.
%% @spec delete_username(ResourceId, Context) -> void
delete_username(Id, Context) when is_integer(Id), Id /= 1 ->
    case z_acl:has_role(admin, Context) orelse z_acl:user(Context) == Id of
        true ->  z_db:q("delete from identity where rsc_id = $1 and type = 'username_pw'", [Id], Context);
        false -> {error, eacces}
    end.


%% @doc Change the username of the resource id, only possible if there is already an username/password set
%% @spec set_username(ResourceId, Username, Context) -> ok | {error, Reason}
set_username(Id, Username, Context) ->
    case z_acl:has_role(admin, Context) orelse z_acl:user(Context) == Id of
        true ->
            F = fun(Ctx) ->
                UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and rsc_id <> $1 and key = $2", [Id, Username], Ctx),
                case UniqueTest of
                    0 ->
                        case z_db:q("update identity set key = $2 where rsc_id = $1 and type = 'username_pw'", [Id, Username], Ctx) of
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
%% @spec set_username_pw(RscId, Username, Password) -> ok | {error, Reason}
set_username_pw(Id, Username, Password, Context) ->
    case z_acl:has_role(admin, Context) orelse z_acl:user(Context) == Id of
        true ->
            Username1 = string:to_lower(Username),
            Hash = hash(Password),
            F = fun(Ctx) ->
                Rupd = z_db:q("
                            update identity 
                            set key = $2,
                                propb = $3,
                                modified = now()
                            where type = 'username_pw' and rsc_id = $1", [Id, Username1, Hash], Ctx),
                case Rupd of
                    0 ->
                        UniqueTest = z_db:q1("select count(*) from identity where type = 'username_pw' and key = $1", [Username], Ctx),
                        case UniqueTest of
                            0 ->
                                z_db:q("insert into identity (rsc_id, is_unique, type, key, propb) values ($1, true, 'username_pw', $2, $3)", [Id, Username1, Hash], Ctx);
                            _Other ->
                                throw({error, eexist})
                        end;
                    1 -> 
                        1
                end
            end,
            case z_db:transaction(F, Context) of
                1 -> ok;
                R -> R
            end;
        false ->
            {error, eacces}
    end.


%% @doc Return the rsc_id with the given username/password.  When succesful then updates the 'visited' timestamp of the entry.
%% @spec check_username_pw(Username, Password, Context) -> {ok, Id} | {error, Reason}
check_username_pw(Username, Password, Context) ->
    Username1 = string:to_lower(Username),
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
%% @spec get_credentials(integer(), context()) -> list()
get_rsc(Id, Context) ->
    z_db:assoc("select * from identity where rsc_id = $1", [Id], Context).

get_rsc(Id, Type, Context) ->
    z_db:assoc_row("select * from identity where rsc_id = $1 and type = '$2'", [Id, Type], Context).


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


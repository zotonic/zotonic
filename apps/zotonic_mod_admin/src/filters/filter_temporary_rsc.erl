%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Creates a temporary resource. If not modified then it will be deleted.

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

-module(filter_temporary_rsc).
-export([
    temporary_rsc/2,
    temporary_rsc/3,

    task_delete_inactive/4
]).

%% Check every hour if the page can be deleted.
-define(INACTIVE_CHECK_DELAY, 3600).
-define(DELETE_TIMEOUT, ?INACTIVE_CHECK_DELAY*1000).

-include_lib("kernel/include/logger.hrl").

temporary_rsc(RscId, Context) ->
    temporary_rsc(RscId, {props, []}, Context).

temporary_rsc(undefined, {props, Props}, Context) when is_list(Props) ->
    make_temporary_rsc(Props, Context);
temporary_rsc(undefined, [{_,_}|_] = Props, Context) ->
    make_temporary_rsc(Props, Context);
temporary_rsc(undefined, [], Context) ->
    make_temporary_rsc([], Context);
temporary_rsc(<<>>, Props, Context) ->
    temporary_rsc(undefined, Props, Context);
temporary_rsc([], Props, Context) ->
    temporary_rsc(undefined, Props, Context);
temporary_rsc(RscId, _Props, _Context) ->
    RscId.

task_delete_inactive(RscId, Key, SessionId, Context) ->
    case is_unmodified_rsc(RscId, Context) of
        true ->
            case z_server_storage:secure_lookup(Key, SessionId, Context) of
                {ok, RscId} ->
                    {delay, ?INACTIVE_CHECK_DELAY};
                _Other ->
                    ?LOG_DEBUG("Deleting unmodified temporary resource ~p", [RscId]),
                    ok = m_rsc:delete(RscId, z_acl:sudo(Context)),
                    ok
            end;
        false ->
            ok
    end.


%% --- internal functions ---

make_temporary_rsc(Props, Context) ->
    make_temporary_rsc( z_context:session_id(Context), Props, Context ).

make_temporary_rsc({error, _}, _Props, _Context) ->
    undefined;
make_temporary_rsc({ok, _SessionId}, Props, Context) ->
    {Cat, Props1} = ensure_category(Props, Context),
    case m_rsc:rid(Cat, Context) of
        undefined ->
            ?LOG_WARNING("filter_temporary_rsc: could not find category '~p'", [Cat]),
            undefined;
        CatId ->
            make_rsc(
                    find_existing(CatId, Context),
                    CatId, Props1,
                    Context)
    end.

make_rsc({ok, RscId}, _CatId, _Props, _Context) ->
    RscId;
make_rsc({error, not_found}, CatId, Props, Context) ->
    case m_rsc:insert(Props, Context) of
        {ok, RscId} ->
            Key = {temporary_rsc, CatId},
            {ok, SessionId} = z_context:session_id(Context),
            {ok, ClientId} = z_context:client_id(Context),
            m_server_storage:secure_store(Key, RscId, Context),
            Args = [
                RscId,
                Key,
                ClientId,
                SessionId
            ],
            z_pivot_rsc:insert_task_after(
                        ?INACTIVE_CHECK_DELAY,
                        ?MODULE, task_delete_inactive, z_convert:to_binary(RscId), Args,
                        Context),
            RscId;
        {error, _} = Error ->
            ?LOG_ERROR("Can not make temporary resource error ~p on ~p", [Error, Props]),
            undefined
    end;
make_rsc({error, _} = Error, _CatId, _Props, _Context) ->
    ?LOG_ERROR("Can not make temporary resource error ~p on storage lookup", [ Error ]),
    undefined.

%% If no user then limit to 1 temporary rsc per client
find_existing(CatId, Context) ->
    case m_server_storage:secure_lookup({temporary_rsc, CatId}, Context) of
        {ok, RscId} when is_integer(RscId) ->
            case is_unmodified_rsc(RscId, Context) of
                true -> {ok, RscId};
                false -> {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _} = Error ->
            Error
    end.

is_unmodified_rsc(Id, Context) ->
    m_rsc:exists(Id, Context) andalso m_rsc:p_no_acl(Id, version, Context) =:= 1.

ensure_category(Props, Context) ->
    case cat(Props, Context) of
        undefined -> {article, [{category, article}|Props]};
        Cat -> {Cat, Props}
    end.

cat(Props, Context) ->
    case z_template_compiler_runtime:find_value(category, Props, #{}, Context) of
        undefined -> z_template_compiler_runtime:find_value(category_id, Props, #{}, Context);
        Cat -> Cat
    end.

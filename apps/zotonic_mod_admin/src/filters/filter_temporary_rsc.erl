%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Creates a temporary resource. If not modified within the session's life time then it will be deleted.

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

    task_delete_inactive/3
]).

%% Check every hour if the page can be deleted.
-define(INACTIVE_CHECK_DELAY, 3600).
-define(DELETE_TIMEOUT, ?INACTIVE_CHECK_DELAY*1000).

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
temporary_rsc("xxx", Props, Context) ->
    temporary_rsc(undefined, Props, Context);
temporary_rsc(RscId, _Props, _Context) ->
    RscId.


task_delete_inactive(RscId, ClientId, Context) ->
    case is_unmodified_rsc(RscId, Context) of
        true ->
            case is_client_alive(ClientId, Context) of
                false ->
                    lager:debug("Deleting unmodified temporary resource ~p", [RscId]),
                    ok = m_rsc:delete(RscId, z_acl:sudo(Context)),
                    ok;
                true ->
                    {delay, ?INACTIVE_CHECK_DELAY}
            end;
        false ->
            ok
    end.


%% --- internal functions ---

make_temporary_rsc(Props, Context) ->
    make_temporary_rsc( z_context:client_id(Context), Props, Context ).

make_temporary_rsc(undefined, _Props, _Context) ->
    undefined;
make_temporary_rsc(ClientId, Props, Context) ->
    {Cat, Props1} = ensure_category(Props, Context),
    case m_rsc:rid(Cat, Context) of
        undefined ->
            lager:warning("filter_temporary_rsc: could not find category '~p'", [Cat]),
            undefined;
        CatId ->
            make_rsc(
                    find_existing(ClientId, CatId, Context),
                    ClientId, CatId, Props1,
                    Context)
    end.

make_rsc({ok, RscId}, _ClientId, _CatId, _Props, _Context) ->
    RscId;
make_rsc({error, notfound}, ClientId, CatId, Props, Context) ->
    case m_rsc:insert(Props, Context) of
        {ok, RscId} ->
            z_session:set({temporary_rsc, CatId}, RscId, Context),
            spawn_client_monitor(RscId, ClientId, Context),
            Args = [
                RscId,
                ClientId
            ],
            z_pivot_rsc:insert_task_after(
                        ?INACTIVE_CHECK_DELAY,
                        ?MODULE, task_delete_inactive, z_convert:to_binary(RscId), Args,
                        Context),
            RscId;
        {error, _} = Error ->
            lager:error("Can not make temporary resource error ~p on ~p", [Error, Props]),
            undefined
    end;
make_rsc({error, _}, _ClientId, _CatId, _Props, _Context) ->
    undefined.


spawn_client_monitor(RscId, ClientId, Context) ->
    case mqtt_sessions_registry:find_session(z_context:site(Context), ClientId, Context) of
        {ok, MqttSessionPid} ->
            ContextAsync = z_context:prune_for_async(Context),
            erlang:spawn(
                    fun() ->
                        client_monitor(RscId, MqttSessionPid, ContextAsync)
                    end);
        {error, _} = Error ->
            Error
    end.

client_monitor(RscId, MqttSessionPid, Context) ->
    erlang:monitor(process, MqttSessionPid),
    receive
        {'DOWN', _MRef, process, MqttSessionPid, _Reason} ->
            delete_if_unmodified(RscId, Context)
        after ?DELETE_TIMEOUT ->
            delete_if_unmodified(RscId, Context)
    end.

delete_if_unmodified(RscId, Context) ->
    case is_unmodified_rsc(RscId, Context) of
        true ->
            lager:debug("Deleting temporary resource ~p due to stopped page session", [RscId]),
            m_rsc:delete(RscId, z_acl:sudo(Context));
        false ->
            ok
    end.


%% If no user then limit to 1 temporary rsc per client
find_existing(undefined, _CatId, _Context) ->
    {error, nosession};
find_existing(_ClientId, CatId, Context) ->
    case z_session:get({temporary_rsc, CatId}, Context) of
        RscId when is_integer(RscId) ->
            case is_unmodified_rsc(RscId, Context) of
                true -> {ok, RscId};
                false -> {error, notfound}
            end;
        undefined ->
            {error, notfound}
    end.

is_unmodified_rsc(Id, Context) ->
    m_rsc:exists(Id, Context) andalso m_rsc:p_no_acl(Id, version, Context) =:= 1.

is_client_alive(ClientId, Context) ->
    case mqtt_sessions_registry:whois(z_context:site(Context), ClientId, Context) of
        {ok, Pid} when is_pid(Pid) -> true;
        {error, _} -> false
    end.

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

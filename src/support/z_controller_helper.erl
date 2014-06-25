%% @author Marc Worrell
%% @copyright 2014 Marc Worrell
%%
%% @doc Helper functions commonly used in controllers.

%% Copyright 2014 Marc Worrell
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

-module(z_controller_helper).
-include("zotonic.hrl").

-export([
    is_authorized/1,
    get_id/1
 ]).


% @doc Check if the current user is is allowed to view the resource.
is_authorized(Context) ->
    case z_context:get(acl, Context) of
        undefined ->
            is_authorized_action(Context);
        ignore ->
            ?WM_REPLY(true, Context);
        is_auth ->
            case z_auth:is_auth(Context) of
                true -> is_authorized_action(Context);
                false -> is_authorized_action(false, Context)
            end;
        logoff ->
            Context1 = case z_auth:is_auth(Context) of
                           true ->
                               z_auth:logoff(Context);
                           false ->
                               Context
                       end,
            is_authorized_action(Context1);
        Acl ->
            is_authorized_action(append_acl(Acl, Context), Context)
    end.

%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int() | undefined
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q("id", Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> undefined
    end.

%%
%% Helpers
%%

is_authorized_action(Context) ->
    is_authorized_action([get_acl_action(Context)], Context).

is_authorized_action(Acl, Context) ->
    %% wm_is_authorized for Acl::{view, undefined} -> true
    z_acl:wm_is_authorized(Acl, Context).

get_acl_action(Context) ->
    {z_context:get(acl_action, Context, view), get_id(Context)}.

append_acl({_, _}=Acl, Context) ->
    [get_acl_action(Context), Acl];
append_acl(Acl, Context) when is_list(Acl) ->
    [get_acl_action(Context) | Acl].


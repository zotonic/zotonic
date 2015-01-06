%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Disconnect an authentication method.

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

-module(action_authentication_auth_disconnect).
-author("Marc Worrell <marc@worrell.nl>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    RscId = z_convert:to_integer(proplists:get_value(id, Args)),
    Type = z_convert:to_binary(proplists:get_value(type, Args)),
    Postback = {auth_disconnect, RscId, Type},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event(#postback{message={auth_disconnect, _RscId, <<"username_pw">>}}, Context) ->
	Context;
event(#postback{message={auth_disconnect, _RscId, <<"email">>}}, Context) ->
	Context;
event(#postback{message={auth_disconnect, RscId, Type}}, Context) when is_binary(Type), is_integer(RscId) ->
    case z_acl:rsc_editable(RscId, Context) of
        true ->
            m_identity:delete_by_type(RscId, Type, Context),
            Context;
        false ->
            z_render:growl(?__("Access denied", Context), Context)
    end.

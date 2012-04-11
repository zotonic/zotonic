%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-28
%% @doc Delete a media file from a resource, no confirmation.

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

-module(action_admin_delete_media).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_media, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a media.  After the deletion the user is redirected, and/or some items on the page are faded out.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={delete_media, Id, OnSuccess}}, Context) ->
    case z_acl:rsc_deletable(Id, Context) of
        true ->
            ok = m_media:delete(Id, Context),
            lists:foldl(
                fun (Act, Ctx) ->
                    z_render:wire(Act, Ctx)
                end,
                Context,
                lists:flatten(OnSuccess));
        false ->
            z_render:growl_error(?__("You are not allowed to delete this media.", Context), Context)
    end.

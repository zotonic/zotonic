%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @doc A media item has been chosen for insertion in the body text.

%% Copyright 2009 Arjan Scherpenisse
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

-module(action_admin_zmedia_choose).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback = {zmedia_choose, Args},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @spec event(Event, Context1) -> Context2
event(#postback{message={zmedia_choose, []}}, Context) ->
    ?DEBUG(z_context:get("media_id", Context)),
    Args = [{id, z_context:get("media_id", Context)}],
    z_render:wire({zmedia_has_chosen, Args}, Context);

%% @spec event(Event, Context1) -> Context2
event(#postback{message={zmedia_choose, Args}}, Context) ->
    z_render:wire({zmedia_has_chosen, Args}, Context).


%z_render:wire([{growl, [{text, "Yay."}]}], Context).



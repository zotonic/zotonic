%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse

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

-module(action_admin_zmedia_has_chosen).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Id   = z_convert:to_list(proplists:get_value(id, Args, "")),
	Script = [<<"z_dialog_close();window.z_choose_zmedia(\"">>,z_utils:js_escape(Id),<<"\");">>],
	{Script, Context}.

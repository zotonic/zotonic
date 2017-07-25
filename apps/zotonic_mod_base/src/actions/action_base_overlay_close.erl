%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Close the overlay

%% Copyright 2016 Marc Worrell
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

-module(action_base_overlay_close).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(_TriggerId, _TargetId, _Args, Context) ->
    {<<"z_dialog_overlay_close();">>, Context}.

%% @author Andreas Stenius <kaos@astekk.se>
%% @copyright 2013 Andreas Stenius
%%
%% Based on code copyright (c) 2008-2009 Rusty Klophaus
%% Based on code copyright (c) 2009 Marc Worrell

%% Copyright 2013 Andreas Stenius
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

-module(action_base_template).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    z_template:render_to_iolist(
      proplists:get_value(template, Args), Args, Context).

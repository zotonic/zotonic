%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Get a "flat" of menu parents

%% Copyright 2013 Marc Worrell
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

-module(filter_menu_rsc).
-export([menu_rsc/2]).

-include("zotonic_notifications.hrl").

menu_rsc(RscId, Context) ->
	case z_notifier:first(#menu_rsc{id=RscId}, Context) of
		undefined -> m_rsc:rid(main_menu, Context);
		none -> undefined;
		MenuId -> m_rsc:rid(MenuId, Context)
	end.


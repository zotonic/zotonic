%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Close the dialog, optionally pass the level of the dialog to be closed.
%% If level 0 is passed then all dialogs are closed.  If no level is passed then
%% the top-most dialog is closed.
%% @end.

%% Copyright 2009-2023 Marc Worrell
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

-module(action_wires_dialog_close).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

render_action(_TriggerId, _TargetId, Args, Context) ->
	Level = case proplists:get_value(level, Args) of
		undefined -> 0;
		<<"top">> -> <<"top">>;
		Lvl -> z_convert:to_integer(Lvl)
	end,
	{<<"z_dialog_close({level:", (integer_to_binary(Level))/binary ,"});">>, Context}.

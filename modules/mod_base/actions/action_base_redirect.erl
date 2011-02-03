%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Perform a redirect to another page.
%% @todo Make this a better redirect by including the host name of the site
%% @todo Allow the location/id types of redirect to have extra arguments

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

-module(action_base_redirect).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Script = case proplists:get_value(back, Args) of
        true ->
            case get_location(proplists:delete(back, Args), Context) of
                undefined ->
                    "history.go(-1);";
                Location ->
                    [<<"if (history.length > 1) history.go(-1); else window.location = \"">>,z_utils:js_escape(Location),$",$;]
            end;
        _ ->
            Location = get_location(Args, Context),
            [<<"window.location = \"">>,z_utils:js_escape(Location),$",$;]
    end,
    {Script, Context}.


get_location(Args, Context) ->
    case proplists:get_value(dispatch, Args) of
        undefined ->
            case proplists:get_value(id, Args) of
                undefined -> 
                    proplists:get_value(location, Args, "/");
                Id ->
                    m_rsc:p(Id, page_url, Context)
            end;
        DispatchString ->
            Dispatch = z_convert:to_atom(DispatchString),
            Args1 = proplists:delete(dispatch, Args),
            z_dispatcher:url_for(Dispatch, Args1, none, Context)
    end.
    
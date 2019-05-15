%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Perform a redirect to another page.

%% Copyright 2009-2017 Marc Worrell
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

-module(action_wires_redirect).
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4,
    redirect_location/2
]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Script = case proplists:get_value(back, Args) of
        true ->
            case get_location(proplists:delete(back, Args), Context) of
                undefined ->
                    <<"history.go(-1);">>;
                Location ->
                    [<<"if (history.length > 1) history.go(-1); else window.location = \"">>,z_utils:js_escape(Location),$",$;]
            end;
        _ ->
            case get_location(Args, Context) of
                undefined ->
                    [];
                Location ->
                    [<<"window.location = \"">>,z_utils:js_escape(Location),$",$;]
            end
    end,
    {Script, Context}.


get_location(Args, Context) ->
    case proplists:get_value(dispatch, Args) of
        undefined ->
            case proplists:lookup(id, Args) of
                none ->
                    proplists:get_value(location, Args, <<"/">>);
                {id, undefined} ->
                    undefined;
                {id, Id} ->
                    m_rsc:p(Id, page_url, Context)
            end;
        DispatchString ->
            Dispatch = z_convert:to_atom(DispatchString),
            Args1 = proplists:delete(dispatch, Args),
            z_dispatcher:url_for(Dispatch, Args1, none, Context)
    end.

%% @doc Return the location for the redirect, iff the location is an url
-spec redirect_location({redirect, list()}, z:context()) -> {ok, binary()} | false.
redirect_location({redirect, Args}, Context) ->
    case get_location(Args, Context) of
        undefined -> false;
        Location -> {ok, iolist_to_binary(Location)}
    end.


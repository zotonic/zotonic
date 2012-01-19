%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Connect a page to a signal

%% Copyright 2010 Maas-Maarten Zeeman
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

-module(action_signal_connect).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-include("zotonic.hrl").
-export([render_action/4, event/2, get_slot/2, store_slot/3, delete_slot/2]).

render_action(TriggerId, TargetId, Args, Context) ->
    Signal = proplists:get_value(signal, Args),
    Actions = proplists:get_all_values(action, Args),
    Name = proplists:get_value(name, Args),

    Postback = {connect, [{signal, Signal}, {name, Name}, {actions, Actions}]},

    %% Genereer postback code voor het emitten van een signal.
    {Script, _Context} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    
    {Script, Context}.

%% @doc Connect a signal to a collection of actions.  
%
event(#postback{message={connect, [{signal, Signal}, {name, Name}, {actions, Actions}]}}, Context) ->
    Slot = z_connect:page(Signal, Actions, Context),
    store_slot(Name, Slot, Context),
    Context.

% @doc Store the slot under a name.
%
store_slot(undefined, _Slot, _Context) ->
    ok;
store_slot(Name, Slot, Context) ->
    PageSlots = case z_context:get_page(slot_names, Context) of
		    undefined -> [];
		    S -> S
		end,
    z_context:set_page(slot_names, [{Name, Slot} | PageSlots], Context).

% @doc Delete the slot with the given name.
%
delete_slot(undefined, _Context) -> ok;
delete_slot(Name, Context) ->
    PageSlots = case z_context:get_page(slot_names, Context) of
		    undefined -> [];
		    S -> S
		end,
    UpdatedPageSlots = lists:keydelete(Name, 1, PageSlots),
    z_context:set_page(slot_names, [UpdatedPageSlots], Context).
    

% @doc retrieve the slot with name
%
get_slot(undefined, _Context) ->
    undefined;
get_slot(Name, Context) ->
    PageSlots = z_context:get_page(slot_names, Context),
    proplists:get_value(Name, PageSlots).


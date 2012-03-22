%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Signal and slot mechanism for use in templates.

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

-module(z_connect).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([page/3]).
-export([slot/3]).

-include("zotonic.hrl").

% @doc Connect the signal to the page, when the signal is emitted, the
% actions are rendered.
% 
page(Signal, Actions, Context) ->
    AsyncContext = z_context:prune_for_async(Context),
    z_context:spawn_link_page(?MODULE, slot, [Signal, Actions, AsyncContext], Context).

%% @doc process which registers itself with the mod_signal module. It will receive a signal
% if somebody emits a matching signal.
%
slot(SignalPrototype, Actions, ConnectorContext) ->
    process_flag(trap_exit, true),
    mod_signal:connect(SignalPrototype, self(), ConnectorContext),
    receive_loop(SignalPrototype, Actions, ConnectorContext).

% @doc Wait for incomging signals. When the signal arrives the actions are rendered, and send
% to the page.
%
receive_loop(SignalPrototype, Actions, ConnectorContext) ->
    receive 
    {signal, Signal, _EmitterContext} ->
        render_page_actions(Signal, Actions, ConnectorContext),
        receive_loop(SignalPrototype, Actions, ConnectorContext);
    {script, Script} ->
        z_context:add_script_page(Script, ConnectorContext),
        receive_loop(SignalPrototype, Actions, ConnectorContext);
    disconnected ->
        disconnected;
    {'EXIT', _From, _Reason} ->
        mod_signal:disconnect(SignalPrototype, self(), ConnectorContext)
    end.

% @doc Render the actions and send the scripts to the page connected to the signal.
%
render_page_actions(Signal, Actions, Context) ->
    {_, SignalProps} = Signal,
    Actions1 = [ {Name,  [ {signal, Signal}, {signal_props, SignalProps} | Props ] } || {Name, Props} <- Actions],
    Options  = [{action, X} || X <- Actions1],

    %% What parameters should be used here?
    Script = z_script:get_script(z_render:wire(undefined, undefined, {event, Options}, Context)),
    
    z_context:add_script_page(Script, Context).

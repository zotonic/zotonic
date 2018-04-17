%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018  Marc Worrell
%% @doc Support for deprecated wires, actions, and transport.

%% Copyright 2018 Marc Worrell
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

-module(mod_wires).

-mod_title("Wires and Actions").
-mod_description("[DEPRECATED] Wires, actions, and other embeddable JavaScript.").
-mod_depends([ mod_mqtt ]).

-export([
    observe_acl_is_allowed/2,
    observe_output_html/3,
    'mqtt:zotonic-transport/+'/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Allow publish from clients to the transport topic
observe_acl_is_allowed(#acl_is_allowed{ action = publish, object = #acl_mqtt{ topic = [ <<"zotonic-transport">>, _ ] } }, _Context) ->
    true;
observe_acl_is_allowed(
    #acl_is_allowed{
        action = publish,
        object = #acl_mqtt{
            topic = [ <<"bridge">>, ClientId, <<"zotonic-transport">>, <<"eval">> ]
        }
    }, #context{ client_id = ClientId }) when is_binary(ClientId) ->
    true;
observe_acl_is_allowed(_, _Context) ->
    undefined.


%% @doc Render nested actions and scomp results.
observe_output_html(#output_html{}, {MixedHtml, Context}, _Context) ->
    z_render:output(MixedHtml, Context).

%% @doc Subscribe to transport events coming in from the client.
'mqtt:zotonic-transport/+'(
    _SubCtx,
    #{
        type := publish,
        message := #{
            type := publish,
            payload := Payload,
            topic := [ _, Delegate ]
        },
        publisher_context := Context
    }) ->
    z_transport:transport(Delegate, Payload, Context).

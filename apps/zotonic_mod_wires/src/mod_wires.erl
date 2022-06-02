%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2020 Marc Worrell
%% @doc Support for wires, actions, and transport.

%% Copyright 2018-2020 Marc Worrell
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

-mod_description("Build interactive user interfaces with template wires, actions, validators and more.").

-mod_depends([mod_mqtt]).

-export([observe_acl_is_allowed/2,
         observe_output_html/3,
         observe_page_actions/2,
         'mqtt:zotonic-transport/+'/2]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

%% @doc Allow publish from clients to the transport topic
observe_acl_is_allowed(#acl_is_allowed{
                           action = publish,
                           object = #acl_mqtt{ topic = [<<"zotonic-transport">>, _] }
                       },
                       _Context) ->
    true;
observe_acl_is_allowed(#acl_is_allowed{
                           action = publish,
                           object = #acl_mqtt{ topic = [<<"bridge">>, ClientId, <<"zotonic-transport">>, <<"eval">>] }
                       },
                       #context{ client_id = ClientId })
    when is_binary(ClientId) ->
    true;
observe_acl_is_allowed(_, _Context) ->
    undefined.

%% @doc Render nested actions and scomp results.
observe_output_html(#output_html{  }, {MixedHtml, Context}, _Context) ->
    z_render:output(MixedHtml, Context).

observe_page_actions(#page_actions{ actions = Actions }, Context) ->
    Context1 = z_render:clean(Context),
    Context2 = z_render:wire(Actions, Context1),
    Script = iolist_to_binary(z_render:get_script(Context2)),
    z_mqtt:publish([<<"~client">>, <<"zotonic-transport">>, <<"eval">>], Script, Context).

%% @doc Subscribe to transport events coming in from the client.
'mqtt:zotonic-transport/+'(#{
                               type := publish,
                               payload := Payload,
                               topic := [_, Delegate]
                           },
                           Context) ->
    z_context:q_upload_keepalive(true, Context),
    z_transport:transport(Delegate, Payload, Context).

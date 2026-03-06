%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018-2025 Marc Worrell
%% @doc Support for wires, actions, and transport.
%% @end

%% Copyright 2018-2025 Marc Worrell
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
-moduledoc("
Actions, tags (also known as screen components), and javascript for user interfaces using *wires*.

Used by [mod_admin](/id/doc_module_mod_admin) and the other administrative modules.

Wires are actions that are directly coupled to user interface elements. These couplings are defined in the templates
using the [wire](/id/doc_template_scomp_scomp_wire#scomp-wire) tag.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_acl_is_allowed`: Allow publish from clients to the transport topic.
- `observe_output_html`: Render nested actions and scomp results using `z_render:output`.
- `observe_page_actions`: Sanitize and normalize page action payloads before they are sent to the browser.
- `observe_postback_notify`: Handle postback notifications for showing a media overlay.
- `observe_scomp_script_render`: Part of the {% script %} rendering in templates using `z_render:make_postback_info`.

Delegate callbacks:

- `event/2` with `postback` messages: `mediaoverlay_update`.

").

-mod_title("Wires and Actions").
-mod_description("Build interactive user interfaces with template wires, actions, validators and more.").
-mod_depends([ mod_mqtt ]).
-mod_prio(1000).

-export([
    event/2,
    observe_postback_notify/2,
    observe_acl_is_allowed/2,
    observe_output_html/3,
    observe_scomp_script_render/2,
    observe_page_actions/2,
    'mqtt:zotonic-transport/+'/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

%% @doc Handle the event for the mediaoverlay.
event(#postback{ message={mediaoverlay_update, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    {ids, Ids} = proplists:lookup(ids, Args),
    {element_id, EltId} = proplists:lookup(element_id, Args),
    {is_next, IsNext} = proplists:lookup(is_next, Args),
    case Ids of
        [_|_] ->
            NewId = if
                IsNext -> find_next(Id, Ids);
                true -> find_prev(Id, Ids)
            end,
            z_render:replace(
                EltId,
                #render{
                    template = "_mediaoverlay.tpl",
                    vars = [
                        {id, NewId},
                        {ids, Ids}
                    ]
                },
                Context);
        _ ->
            Context
    end.

find_next(Id, Ids) ->
    case lists:dropwhile(fun(X) -> X =/= Id end, Ids) of
        [ _, Next | _ ] -> Next;
        _ -> hd(Ids)
    end.

find_prev(Id, Ids) ->
    find_next(Id, lists:reverse(Ids)).


%% @doc Handle the lightbox event, filter the id and ids, open an overlay.
observe_postback_notify(#postback_notify{ message = <<"mediaoverlay">> }, Context) ->
    Ids = case z_context:get_q(<<"ids">>, Context) of
        L when is_list(L) -> L;
        _ -> []
    end,
    Ids1 = lists:filtermap(
        fun(QId) ->
            case m_rsc:rid(QId, Context) of
                undefined -> false;
                RId ->
                    case z_acl:rsc_visible(RId, Context) of
                        true -> {true, RId};
                        false -> false
                    end
            end
        end,
        Ids),
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    Id1 = case z_acl:rsc_visible(Id, Context) of
        true -> Id;
        false when Ids1 =/= [] -> hd(Ids1);
        false -> undefined
    end,
    if
        is_integer(Id1) ->
            Vars = [
                {id, Id1},
                {ids, Ids1},
                {class, "overlay-mediaoverlay"}
            ],
            z_render:overlay("_mediaoverlay.tpl", Vars, Context);
        true ->
            % Ignore, nothing to show.
            undefined
    end;
observe_postback_notify(#postback_notify{}, _Context) ->
    undefined.

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

%% @doc Part of the {% script %} rendering in templates
observe_scomp_script_render(#scomp_script_render{ is_nostartup = false }, Context) ->
    DefaultFormPostback = z_render:make_postback_info(<<>>, <<"submit">>, undefined, undefined, undefined, Context),
    [
        <<"z_init_postback_forms();\nz_default_form_postback = \"">>, DefaultFormPostback, $", $;,
        <<"if (typeof zotonic.wiresReadyResolve == 'function') { zotonic.wiresReadyResolve(); }">>
    ];
observe_scomp_script_render(#scomp_script_render{ is_nostartup = true }, _Context) ->
    [].

observe_page_actions(#page_actions{ actions = Actions }, Context) ->
    Context1 = z_render:clean(Context),
    Context2 = z_render:wire(Actions, Context1),
    Script = iolist_to_binary( z_render:get_script(Context2) ),
    z_mqtt:publish([ <<"~client">>, <<"zotonic-transport">>, <<"eval">> ], Script, Context).

%% @doc Subscribe to transport events coming in from the client.
'mqtt:zotonic-transport/+'( #{
        type := publish,
        payload := Payload,
        topic := [ _, Delegate ]
    } = Msg,
    Context) ->
    z_context:logger_md(Context),
    z_context:q_upload_keepalive(true, Context),
    z_transport:transport(Delegate, Payload, Context),
    maybe_send_ack(Msg, Context).

maybe_send_ack(#{ properties := #{ response_topic := RespTopic }}, Context) when is_binary(RespTopic); is_list(RespTopic) ->
    z_mqtt:publish(RespTopic, true, Context);
maybe_send_ack(_Msg, _Context) ->
    ok.

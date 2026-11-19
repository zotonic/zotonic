%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Administrator editor for traffic guard IP allow and block lists.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(controller_admin_traffic_guard).
-export([service_available/1,is_authorized/1,process/4,event/2]).
-include_lib("zotonic_core/include/zotonic.hrl").
-moduledoc(#{zotonic_keywords => ["reference", "controller", "security", "configuration"]}).
-moduledoc("Administrator-only IP allow and block list editor. Responses are not cached or indexed. Save events call the model, which separately enforces administrator and writable credentials and validates all entries before saving.").

service_available(C) -> {true,z_context:set_noindex_header(z_context:set_nocache_headers(C))}.
is_authorized(C) -> z_controller_helper:is_authorized([{use,mod_admin_config}],C).
process(_,_,_,C) ->
    case m_traffic_guard:lists(C) of
        {ok,L} ->
            Throttled=m_traffic_guard:throttled(C),
            z_context:output(z_template:render("admin_traffic_guard.tpl",
                [{entries,L},{throttled,case Throttled of {ok,T}->T; _->[] end},
                 {guard_unavailable,element(1,Throttled)=:=error},
                 {read_only,z_acl:is_read_only(C)},{current_ip,current_ip(C)}],C),C);
        _ -> z_context:output(<<"Access denied">>,C)
    end.
event(#postback{message=add},C) -> entry_dialog(undefined,undefined,C);
event(#postback{message=add_current},C) -> entry_dialog(undefined,current_ip(C),C);
event(#postback{message={edit,Args}},C) -> entry_dialog(proplists:get_value(id,Args),undefined,C);
event(#submit{message=save},C) ->
    respond(m_traffic_guard:put(z_context:get_q(<<"kind">>,C),z_context:get_q(<<"ips">>,C),
        z_context:get_q(<<"note">>,C),validity(C),C),C);
event(#submit{message=block},C) ->
    respond(m_traffic_guard:block(z_context:get_q(<<"ip">>,C),z_context:get_q(<<"validity">>,C),C),C);
event(#postback{message={delete,Args}},C) ->
    respond(m_traffic_guard:delete(proplists:get_value(id,Args),C),C).

entry_dialog(Id,IPs,C) ->
    case z_acl:is_admin(C) andalso not z_acl:is_read_only(C) of
        false -> respond({error,eacces},C);
        true when Id =:= undefined ->
            z_render:dialog(?__("Add IP entries",C),"_dialog_traffic_guard_entry.tpl",[{ips,IPs}],C);
        true ->
            {ok,Rows}=m_traffic_guard:lists(C),
            case [R || R<-Rows,maps:get(source,R)=:=<<"site">>,maps:get(id,R)=:=Id] of
                [Entry] -> z_render:dialog(?__("Edit IP entry",C),"_dialog_traffic_guard_entry.tpl",[{entry,Entry}],C);
                [] -> z_render:growl_error(?__("This entry is no longer available. Refresh the page.",C),C)
            end
    end.

validity(C) ->
    case z_context:get_q(<<"until">>,C) of
        undefined -> z_context:get_q(<<"validity">>,C);
        <<>> -> z_context:get_q(<<"validity">>,C);
        Date -> Date
    end.
respond(ok,C) -> z_render:wire({reload,[]},C);
respond({error,eacces},C) -> z_render:growl_error(?__("You are not allowed to change these entries.",C),C);
respond({error,not_throttled},C) -> z_render:growl_error(?__("This IP is no longer throttled. Refresh the page.",C),C);
respond({error,too_many_entries},C) -> z_render:growl_error(?__("Each list can contain at most 256 entries.",C),C);
respond(_,C) -> z_render:growl_error(?__("Enter valid IP addresses, a note and a future expiry date or duration. No changes were saved.",C),C).


current_ip(C) ->
    case m_req:get(peer_ip,C) of
        undefined -> undefined;
        IP -> list_to_binary(inet:ntoa(m_traffic_guard:normalize(IP)))
    end.

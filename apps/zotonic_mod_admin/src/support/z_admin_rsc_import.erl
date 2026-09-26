%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%%
%% @doc Support for admin tasks around non authoritative resources.
%% @end

%% Copyright 2021 Marc Worrell
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

-module(z_admin_rsc_import).
-moduledoc("Admin re-import options; all operations require resource edit permission.").

-export([
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message = {import_options, Args}}, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    case z_acl:rsc_editable(Id, Context) of
        false -> z_render:growl_error(?__("You are not allowed to edit this page.", Context), Context);
        true ->
            Options = case m_rsc_import:get_import_status(Id, Context) of
                {ok, Status} -> maps:get(<<"options">>, Status, []);
                _ -> []
            end,
            z_render:dialog(?__("Fetch new version", Context), "_dialog_rsc_import_options.tpl",
                [{id, Id}, {import_options, Options}], Context)
    end;
event(#submit{message = {import_refresh, Args}}, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    case z_acl:rsc_editable(Id, Context) of
        false -> z_render:growl_error(?__("You are not allowed to edit this page.", Context), Context);
        true ->
            Edges = case z_context:get_q(<<"z_import_edges">>, Context) of
                <<"1">> -> 1;
                <<"10">> -> 10;
                _ -> 0
            end,
            Options = [
                {is_forced_update, true},
                {import_edges, Edges},
                {is_subscribe_haspart, z_convert:to_bool(z_context:get_q(<<"z_import_subscribe_haspart">>, Context))},
                {is_no_medium_download, not z_convert:to_bool(z_context:get_q(<<"import_medium">>, Context))}],
            case m_rsc_import:reimport_recursive_async(Id, Options, Context) of
                {ok, _} ->
                    case z_module_manager:active(mod_websub, Context) of
                        true ->
                            case z_convert:to_bool(z_context:get_q(<<"z_import_subscribe">>, Context)) of
                                true -> m_websub:subscribe(Id, Context);
                                false -> m_websub:unsubscribe(Id, Context)
                            end;
                        false -> ok
                    end,
                    z_render:wire({reload, []}, z_render:dialog_close(Context));
                {error, _} -> z_render:growl_error(?__("Error importing page from the remote server.", Context), Context)
            end
    end;
event(#postback{ message={import_refresh, Args} }, Context) ->
    OnError = proplists:get_value(on_error, Args),
    {id, Id} = proplists:lookup(id, Args),
    Options = [
        {is_forced_update, true},
        {is_no_medium_download, false}
    ],
    case m_rsc_import:reimport_recursive_async(Id, Options, Context) of
        {ok, {_Id, _ObjectIds}} ->
            case proplists:get_all_values(on_success, Args) of
                [] ->
                    z_render:growl(?__("Succesfully imported page from the remote server.", Context), Context);
                OnSuccess ->
                    z_render:wire(OnSuccess, Context)
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error on reimport of resource">>,
                in => zotonic_mod_admin,
                rsc_id => Id,
                result => error,
                reason => Reason
            }),
            Context1 = z_render:wire(OnError, Context),
            z_render:growl_error(?__("Error importing page from the remote server.", Context1), Context1)
    end.

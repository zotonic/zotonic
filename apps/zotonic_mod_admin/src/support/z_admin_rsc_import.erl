%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%%
%% @doc Support for admin tasks around non authoritative resources.

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

-export([
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{ message={import_refresh, Args} }, Context) ->
    OnError = proplists:get_value(on_error, Args),
    {id, Id} = proplists:lookup(id, Args),
    case m_rsc_import:reimport_recursive_async(Id, Context) of
        {ok, {_Id, _ObjectIds}} ->
            case proplists:get_all_values(on_success, Args) of
                [] ->
                    z_render:growl(?__("Succesfully imported page from the remote server.", Context), Context);
                OnSuccess ->
                    z_render:wire(OnSuccess, Context)
            end;
        {error, Reason} ->
            lager:error("Error on reimport of rsc ~p: ~p", [ Id, Reason ]),
            Context1 = z_render:wire(OnError, Context),
            z_render:growl_error(?__("Error importing page from the remote server.", Context1), Context1)
    end.

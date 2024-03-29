%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011-2023 Arjan Scherpenisse
%% @doc Mailing status/control page
%% @end

%% Copyright 2011-2023 Arjan Scherpenisse
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

-module(controller_admin_mailing_status).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    service_available/1,
    resource_exists/1,
    is_authorized/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    {Context2, Id} = controller_admin_edit:ensure_id(Context),
    z_controller_helper:is_authorized([ {use, mod_mailinglist}, {view, Id} ], Context2).


resource_exists(Context) ->
    {Context2, Id} = controller_admin_edit:ensure_id(Context),
    case Id of
        undefined -> {false, Context2};
        _N -> {m_rsc:exists(Id, Context2), Context2}
    end.


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Vars = [
        {id, z_context:get(id, Context)}
    ],
    Html = z_template:render({cat, <<"admin_mailing_status.tpl">>}, Vars, Context),
	z_context:output(Html, Context).

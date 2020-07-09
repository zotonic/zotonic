%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2020 Marc Worrell
%% @doc Redirect to the authorize uri of external identity provider

%% Copyright 2010-2020 Marc Worrell
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

-module(controller_oauth2_service_authorize).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
    ]).

-define(SECRET_DATA_TTL, 3600).

-include_lib("zotonic_core/include/zotonic.hrl").

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    StateId = z_ids:id(),
    ServiceMod = z_context:get(service_module, Context),
    case redirect_location(StateId, ServiceMod, Context) of
        {ok, #{ url := Location } = Redir} when is_binary(Location) ->
            ServiceData = maps:get(data, Redir, undefined),
            StateData = {StateId, ServiceMod, ServiceData, z_context:get_q_all_noz(Context)},
            Expires = termit:expiring(StateData, ?SECRET_DATA_TTL),
            Secret = z_context:state_cookie_secret(Context),
            Encoded = termit:encode_base64(Expires, Secret),
            Args = #{
                oauth_step => <<"authorize">>,
                oauth_state_id => StateId,
                oauth_state => Encoded,
                authorize_url => Location
            },
            Vars = #{
                service_name => ServiceMod:title(Context),
                worker_args => Args
            },
            z_template:render_to_iolist("logon_service_oauth.tpl", Vars, Context);
        {error, _} = Error ->
            lager:error("Error with OAuth redirect for ~p: ~p", [ ServiceMod, Error ]),
            Vars = #{
                service => ServiceMod:title(Context)
            },
            z_template:render_to_iolist("logon_service_error.tpl", Vars, Context)
    end.

redirect_location(StateId, ServiceMod, Context) ->
    RedirectUrl = z_context:abs_url(
        z_dispatcher:url_for(oauth2_service_redirect, Context),
        Context),
    ServiceMod:authorize_url(RedirectUrl, StateId, Context).

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2020 Marc Worrell
%% @doc Handle the OAuth redirect of the OAuth logon handshake.

%% Copyright 2014-2020 Marc Worrell
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

-module(controller_oauth2_service_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Args = #{
        oauth_step => <<"redirect">>,
        oauth_state_id => z_context:get_q(<<"state">>, Context)
    },
    Vars = #{
        worker_args => Args
    },
    z_template:render_to_iolist("logon_service_oauth_done.tpl", Vars, Context).


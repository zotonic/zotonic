%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-03
%% @doc Get information about the system.

%% Copyright 2009 Arjan Scherpenisse
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

-module(service_base_info).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Basic information about the system.").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    Result = case z_auth:is_auth(Context) of
                 true ->
                     z_convert:to_list(z_trans:lookup_fallback(m_rsc:p(Context#context.user_id, title, Context), Context));
                 false ->
                     "Anonymous"
    end,
    {struct, [{"user", {struct, [{"user_name", z_convert:to_atom(Result)},
                                 {"user_id",   Context#context.user_id}]}},
              {"site", {struct, [{"zotonic_version", ?ZOTONIC_VERSION},
                                 {"language",        cfg(i18n, language, Context)}]}}
             ]
    }.

cfg(Key, Value, Context) ->
    z_convert:to_atom(m_config:get_value(Key, Value, Context)).

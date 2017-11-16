%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2017 Arjan Scherpenisse
%% @doc Get information about the system.

%% Copyright 2009-2017 Arjan Scherpenisse
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
-svc_needauth(false).

-export([process_get/1]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_release.hrl").


process_get(Context) ->
    #{
        "user" => user_info(z_acl:user(Context), Context),
        "site" => site_info(z_acl:is_admin(Context), Context)
    }.

user_info(UserId, Context) when is_integer(UserId) ->
    {Name, _Context} = z_template:render_to_iolist("_name.tpl", [{id, UserId}], Context),
    #{
        "user_name" => iolist_to_binary(Name),
        "user_id" => UserId
    };
user_info(undefined, _Context) ->
    #{
        "user_name" => null,
        "user_id" => null
    }.

site_info(true, Context) ->
    #{
        "zotonic_version" => ?ZOTONIC_VERSION,
        "language" => z_language:default_language(Context)
    };
site_info(false, Context) ->
    #{
        "language" => z_language:default_language(Context)
    }.

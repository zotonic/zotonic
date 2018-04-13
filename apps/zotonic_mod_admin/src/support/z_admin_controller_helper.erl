%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_controller helper.

%% Copyright 2009, 2014 Tim Benniks, Maas-Maarten Zeeman
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

-module(z_admin_controller_helper).

-export([
    init_session/1,
    is_authorized/2
]).

init_session(Context) ->
    Context1 = z_context:ensure_qs(Context),
    z_context:lager_md(Context1),
    Context1.

is_authorized(DefaultMod, Context) ->
    Context1 = z_context:set_resp_header(<<"x-frame-options">>, <<"SAMEORIGIN">>, Context),
    Context2 = init_session(Context1),
    z_acl:wm_is_authorized([{use, z_context:get(acl_module, Context, DefaultMod)}], admin_logon, Context2).


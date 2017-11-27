%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Model for mod_admin

%% Copyright 2017 Marc Worrell
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

-module(m_admin).

-export([ m_get/2 ]).

m_get([ pivot_queue_count | Rest ], Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true -> {z_pivot_rsc:queue_count(Context), Rest};
        false -> {undefined, Rest}
    end;
m_get([ rsc_dialog_is_published | Rest ], Context) ->
    {m_config:get_boolean(mod_admin, rsc_dialog_is_published, Context), Rest}.


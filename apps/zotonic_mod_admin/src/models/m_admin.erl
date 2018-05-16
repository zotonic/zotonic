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

-export([ m_get/3 ]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ pivot_queue_count | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true -> {ok, {z_pivot_rsc:queue_count(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ rsc_dialog_is_published | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, rsc_dialog_is_published, Context), Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

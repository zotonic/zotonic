%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2023 Marc Worrell
%% @doc Model for mod_admin
%% @end

%% Copyright 2017-2023 Marc Worrell
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
-moduledoc("
This model exposes some meta-information for the use in `mod_admin` templates.

There are two properties available:

1.   Pivot queue count
     
     Retrieve the count of the pivot queue, the queue which decides which resources need re-indexing.
     
     To view the number of items in the pivot queue, do the following:
     
     
     ```erlang
     {% print m.admin.pivot_queue_count %}
     ```
2.   Default `published` setting for the new-resource dialog
     
     \\{% if m.admin.rsc\\_dialog\\_is\\_published %\\} ... \\{% endif %\\}
").

-export([ m_get/3 ]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"pivot_queue_count">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            Res = #{
                <<"backlog">> => z_pivot_rsc:queue_count_backlog(Context),
                <<"total">> => z_pivot_rsc:queue_count(Context)
            },
            {ok, {Res, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"rsc_dialog_is_published">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, rsc_dialog_is_published, Context), Rest}};
m_get([ <<"rsc_dialog_is_dependent">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, rsc_dialog_is_dependent, Context), Rest}};
m_get([ <<"rsc_dialog_hide_dependent">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, rsc_dialog_hide_dependent, Context), Rest}};
m_get([ <<"edge_list_max_length">> | Rest ], _Msg, Context) ->
    Len = case m_config:get_value(mod_admin, edge_list_max_length, Context) of
        <<>> -> undefined;
        LenS -> z_convert:to_integer(LenS)
    end,
    {ok, {Len, Rest}};
m_get([ <<"connect_created_me">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, connect_created_me, Context), Rest}};
m_get([ <<"is_notrack_refers">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_admin, is_notrack_refers, Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

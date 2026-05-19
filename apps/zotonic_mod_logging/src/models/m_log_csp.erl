%% @author Marc Worrell
%% @copyright 2026 Marc Worrell
%% @doc Model for CSP log messages.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_log_csp).
-moduledoc("
Model for admin Content-Security Report log retrieval by index, gated by admin permissions.

This lists the most recent CSP violation reports received by the controller `controller_csp_report`.


Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/reports/...` | Return the log of recent content security reports; admin-only access. |

`A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    reports/1
]).

m_get([ <<"reports">> | Rest ], _Msg, Context) ->
    case z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_logging, Context) of
        true ->
            case reports(Context) of
                {ok, Rs} ->
                    {ok, {Rs, Rest}};
                {error, _} ->
                    {ok, {[], Rest}}
            end;
        false ->
            {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

reports(Context) ->
    mod_logging:csp_reports(Context).

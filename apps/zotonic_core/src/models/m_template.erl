%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2026 Marc Worrell
%% @doc Render templates
%% @end

%% Copyright 2019-2026 Marc Worrell
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

-module(m_template).
-moduledoc("
Model for dynamic template rendering. It renders a template path passed in the request path and returns the rendered template output.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/render/...` | Treat segments after `/render/` as the template path, render that template with the payload query args, and return the rendered template output. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3
]).

-include_lib("zotonic.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"render">> | TemplatePath ], Msg, Context) when is_map(Msg) ->
    Template = to_template(TemplatePath),
    Payload = case maps:get(payload, Msg, #{}) of
        undefined -> #{};
        L when is_list(L) -> L;
        M when is_map(M) -> M;
        V -> #{ <<"payload">> => V }
    end,
    Context1 = z_context:add_q(Payload, Context),
    {Tpl, _} = z_template:render_to_iolist(Template, [], Context1),
    {ok, {iolist_to_binary(Tpl), []}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

to_template([]) -> <<>>;
to_template(List) ->
    L1 = lists:map(fun z_convert:to_binary/1, List),
    L2 = lists:filter(fun(B) -> B =/= <<>> end, L1),
    iolist_to_binary( lists:join($/, L2) ).

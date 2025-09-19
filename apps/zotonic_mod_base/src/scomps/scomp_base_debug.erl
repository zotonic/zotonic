%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse
%% Date: 2011-02-26
%% @doc Show all assigned variables in the template.

%% Copyright 2011 Arjan Scherpenisse
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

-module(scomp_base_debug).
-moduledoc("
Shows which variables are assigned for use in the current template’s scope:


```erlang
{% debug %}
```



Optionally, variable names can be provided to be debugged:


```erlang
{% debug session_id q template zotonic_dispatch %}
```



By default, all key nodes are collapsed. Expanded, each node contains the value associated and highlighted as Erlang code:


```erlang
.. image:: /img/scomp_base_debug_expanded.png
```

The debug scomp contains three buttons at the top right:

*   `_`: Collapse all key nodes;
*   `□`: Expand all key nodes;
*   `×`: Removes the debug element from the HTML document.

There is also a resizer at the bottom right corner to resize the `debug` element horizontally.

See also

[print](/id/doc_template_tag_tag_print)
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render([], #{} = Vars, Context) ->
    do_render(ensure_q(Vars, Context), Context);
render(Params, #{} = Vars, Context) ->
    Vars1 = lists:foldl(
        fun({K, _}, Acc) ->
            case get_key(K, Vars, Context) of
                {ok, V} ->
                    Acc#{ K => V };
                error ->
                    Acc
            end
        end,
        #{},
        Params),
    do_render(Vars1, Context).

do_render(Vars, Context) ->
    Vars1 = [{K, io_lib:format("~p", [V])} || {K, V} <- maps:to_list(Vars)],
    {ok, z_template:render("_debug.tpl", [{vars, Vars1}], Context)}.

ensure_q(Vars, Context) ->
    case maps:is_key(q, Vars) orelse maps:is_key(<<"q">>, Vars) of
        true ->
            Vars;
        false ->
            Vars#{
                q => z_context:get_q_all(Context)
            }
    end.

get_key(q, #{ <<"q">> := Q }, _Context) ->
    Q;
get_key(q, #{ q := Q }, _Context) ->
    Q;
get_key(q, _Vars, Context) ->
    {ok, z_context:get_q_all(Context)};
get_key(K, Vars, _Context) ->
    case maps:find(K, Vars) of
        error ->
            maps:find(z_convert:to_binary(K), Vars);
        {ok, _} = V ->
            V
    end.


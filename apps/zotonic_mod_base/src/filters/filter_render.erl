%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Render a template. Pass the template variables in the extra argument.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_render).
-moduledoc("
Render a template.

Example:


```django
{% include \"_email.tpl\" name=\"_name.tpl\"|render:%{ id: recipient_id } %}
```

This renders the template `_name.tpl` with the argument `id`.

If the argument is only single id, then it can be passed at once and will be assigned to the `id` argument. The
following is equivalent to the example above:


```django
{% include \"_email.tpl\" name=\"_name.tpl\"|render:recipient_id %}
```
").

-export([
    render/2,
    render/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render(Template, Context) ->
    render(Template, #{}, Context).

render({cat, Args}, Vars, Context) when is_list(Vars); is_map(Vars) ->
    {template, Template} = proplists:lookup(template, Args),
    Vars1 = case proplists:lookup(id, Args) of
        {id, Id} -> set_arg(Vars, id, Id);
        none -> Vars
    end,
    {Data, _Context} = z_template:render_to_iolist({cat, Template}, Vars1, Context),
    {Output, _} = z_render:output(Data, Context),
    iolist_to_binary(Output);
render(Template, Vars, Context) when is_list(Vars); is_map(Vars) ->
    case z_module_indexer:find(template, Template, Context) of
        {ok, Index} ->
            {Data, _Context} = z_template:render_to_iolist(Index, Vars, Context),
            {Output, _} = z_render:output(Data, Context),
            iolist_to_binary(Output);
        {error, Reason} ->
            ?LOG_WARNING(#{
                in => zotonic_mod_base,
                text => <<"Template for render filter not found">>,
                result => error,
                reason => Reason,
                template => Template
            }),
            <<>>
    end.

set_arg(Vars, K, V) when is_list(Vars) ->
    [ {K, V} | proplists:delete(K, Vars) ];
set_arg(Vars, K, V) when is_map(Vars) ->
    K1 = z_convert:to_binary(K),
    Vars1 = maps:remove(K, Vars),
    Vars1#{
        K1 => V
    }.


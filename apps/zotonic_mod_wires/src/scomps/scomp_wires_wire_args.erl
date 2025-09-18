%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Wire an action to an element, adding all non-scomp arguments to the actions before rendering them.

%% Copyright 2009 Marc Worrell
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

-module(scomp_wires_wire_args).
-moduledoc("
Add extra arguments to wired actions.

The tag `wire_args` is used to append extra arguments to actions before wiring those actions. This is useful when an
action is handed down as an argument to a template but still needs the id of a local element.

For example:


```erlang
{% with {my_action some_arg=some_value} as my_action %}
  {% for n in [1,2,3,4] %}
    <a id=\"{{#id.n}}\" href=\"#\">click {{n}}</a>
    {% wire_args action=my_action the_link_id=#id.n %}
  {% endfor %}
{% endwith %}
```

This wires the action `{my_action some_arg=some_value the_link_id=...}` to the links.

The following arguments are part of the wire tag and can’t be used for argument appending: “id”, “type”,
“target”, “action”, “postback” and “delegate”.

See also

the [wire](/id/doc_template_scomp_scomp_wire#scomp-wire) tag.
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id        = proplists:get_value(id, Params, <<>>),
    Type      = proplists:get_value(type,Params,click),
    TargetId  = proplists:get_value(target,Params,Id),
    Actions   = proplists:get_all_values(action,Params),
    Postback  = proplists:get_value(postback,Params),
    Delegate  = proplists:get_value(delegate,Params),

    Args = lists:foldl(fun proplists:delete/2, Params, [id, type, target, action, postback, delegate]),

    Actions1  = lists:flatten(Actions),
    Options   = [{action, append_args(X, Args)} || X <- Actions1, X =/= undefined ],
    Options1  = case Postback of
                	undefined -> Options;
                	Postback  -> [{postback,Postback} | Options]
                end,

    Delegate1 = case Delegate of
        undefined -> undefined;
        _ -> z_convert:to_atom(Delegate)
    end,

    case Options1 of
        [_|_] -> {ok, z_render:wire(Id, TargetId, {event,[{type,Type},{delegate,Delegate1}|Options1]}, Context)};
        _ -> {ok, Context}
    end.

append_args({Action, ActionArgs}, Args) ->
    {Action, ActionArgs ++ Args}.

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Unmask an element previously masked by the mask action

%% Copyright 2010 Marc Worrell
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

-module(action_wires_unmask).
-moduledoc("
See also

action [mask](/id/doc_template_action_action_mask).

Removes a mask that was placed over an element using the [mask](/id/doc_template_action_action_mask) action.

Example:


```django
{% wire action={unmask target=\"logon_outer\"} %}
```

In this example the mask over the logon\\_outer div will be removed.
").

-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Selector = case proplists:get_value(body, Args) of
        true -> "body";
        _ ->
            case z_render:css_selector(TargetId, Args) of
                <<>> -> "body";
                S -> S
            end
    end,
    Script = case Selector of
        undefined -> <<>>;
        <<>> -> <<>>;
        "" -> <<>>;
        _ ->
            [ <<"try { ">>,z_render:render_css_selector(Selector),<<".unmask(); } catch (e) {};">>]
    end,
    {Script, Context}.

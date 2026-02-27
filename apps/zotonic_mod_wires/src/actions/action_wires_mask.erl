%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Mask an element or the whole page with a busy indicator.

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

-module(action_wires_mask).
-moduledoc("
Places a mask over an element, useful for blocking user interaction during lengthy postbacks.

Example:


```html
<a id=\"{{ #fb_logon }}\" href=\"#facebook\">
   <img src=\"/lib/images/fb-login-button.png\" width=\"154\" height=\"22\" alt=\"Facebook login button\">
</a>
{% wire id=#fb_logon
   action={mask target=\"logon_outer\" message=\"Waiting for Facebook .\"}
   action={redirect dispatch=\"facebook_authorize\"} %}
```

In this example the logon_outer div will be masked while the browser is being redirected to Facebook.

Form postbacks are automatically masked.

Note that you need to include a css and a js file to have working masks:


```django
{% lib \"css/jquery.loadmask.css\" %}
{% lib \"js/modules/jquery.loadmask.js\" %}
```

This action takes three possible arguments:

| Argument | Description                                                                      | Example                |
| -------- | -------------------------------------------------------------------------------- | ---------------------- |
| body     | Mask the whole page                                                              | body                   |
| target   | The id of the element to be masked.                                              | target=”search-form”   |
| message  | Message to show next to the spinner image.                                       | message=”Searching...” |
| delay    | Delay (in milliseconds) before the mask is shown. Only shows the mask during lengthy actions. | delay=200              |

See also

action [unmask](/id/doc_template_action_action_unmask), [mask_progress](/id/doc_template_action_action_mask_progress)").
-include_lib("zotonic_core/include/zotonic.hrl").
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
    Message = proplists:get_value(message, Args, ""),
    Delay = proplists:get_value(delay, Args, 0),
    Script = [ <<"try { ">>, z_render:render_css_selector(Selector) ,<<".mask('">>,
                z_utils:js_escape(Message), $',$,,
                z_convert:to_list(Delay),
                <<"); } catch (e) {};">>],
    {Script, Context}.

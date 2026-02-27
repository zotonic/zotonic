%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% Date: 2009-07-16
%% @doc Replace the content of the target with the result of a render action.

%% Copyright 2009-2015 Marc Worrell
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

-module(action_wires_update).
-moduledoc("
Updates the content of an HTML element with a template or a literal HTML text.

Example:


```django
<div id=\"mydiv\"><p>Bye Bye.</p></div>
{% button text=\"hello\" action={update target=\"mydiv\" text=\"<p>Hello World!</p>\"} %}
```

When clicked, the contents of the div will be set to the HTML fragment &lt;p>Hello World!</p>. This replaces any content present.

Note

Use the [update_iframe](/id/doc_template_action_action_update_iframe) action for updating the contents of an `iframe` element.

Another example, now rendering a template:


```django
<ul id=\"mylist\"><li>Some item</li></li>
{% button text=\"hello\" action={update target=\"mylist\" template=\"_list_item.tpl\" id=42} %}
```

This updates the &lt;ul/> with the output of the template \\_list_item.tpl. All arguments to the update action are
also arguments to the template.

| Argument      | Description                                                                      | Example                       |
| ------------- | -------------------------------------------------------------------------------- | ----------------------------- |
| target        | The id of the element receiving the rendered HTML.                               | target=”my-view”              |
| text          | Literal HTML text to be inserted, no escaping will be done.                      | text=”Hello &lt;b>World</b>”  |
| template      | Name of the template to be rendered.                                             | template=”\\\\_list\\\\_view.tpl” |
| include\\\\_all | Add this argument to include all templates with the same name. If not added then the best template will be used. | include\\\\_all                 |
| catinclude    | Add this argument to use a [catinclude](/id/doc_template_tag_tag_catinclude) instead of a normal include of the template. The id argument *must* be present for a catinclude to work. | catinclude id=1               |

All other arguments are passed as-is to the included template(s).

See also

actions [update_iframe](/id/doc_template_action_action_update_iframe),
[insert_top](/id/doc_template_action_action_insert_top) and [insert_bottom](/id/doc_template_action_action_insert_bottom).").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4,
    render_update/5,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    render_update(update, TriggerId, TargetId, Args, Context).

render_update(Method, TriggerId, TargetId, Args, Context) ->
    Html = case proplists:get_value(template, Args) of
        undefined ->
            proplists:get_value(text, Args, "");
        Template ->
            IsAll = proplists:get_value('include_all', Args),
            case z_convert:to_bool(proplists:get_value('catinclude', Args)) of
                true ->
                    #render{template={cat, Template}, vars=Args, is_all=IsAll};
                false ->
                    #render{template=Template, vars=Args, is_all=IsAll}
            end
    end,
    case {TargetId, Html} of
        {undefined,_} -> render_inline(Method, TargetId, Html, Args, Context);
        {_,#render{}} -> render_postback(Method, TriggerId, TargetId, Html, Args, Context);
        {_,_} -> render_static(Method, TargetId, Html, Args, Context)
    end.

render_inline(Method, TargetId, Html, Args, Context) ->
    Sel = z_render:css_selector(TargetId, Args),
    case proplists:get_value(appear, Args) of
        true ->
            case Method of
                update        -> {[], z_render:appear_selector(Sel, Html, Context)};
                replace       -> {[], z_render:appear_replace_selector(Sel, Html, Context)};
                insert_top    -> {[], z_render:appear_top_selector(Sel, Html, Context)};
                insert_bottom -> {[], z_render:appear_bottom_selector(Sel, Html, Context)};
                insert_before -> {[], z_render:appear_before_selector(Sel, Html, Context)};
                insert_after  -> {[], z_render:appear_after_selector(Sel, Html, Context)};
                iframe        -> {[], z_render:update_iframe(TargetId, Html, Context)}
            end;
        _ ->
            case Method of
                update        -> {[], z_render:update_selector(Sel, Html, Context)};
                replace       -> {[], z_render:replace_selector(Sel, Html, Context)};
                insert_top    -> {[], z_render:insert_top_selector(Sel, Html, Context)};
                insert_bottom -> {[], z_render:insert_bottom_selector(Sel, Html, Context)};
                insert_before -> {[], z_render:insert_before_selector(Sel, Html, Context)};
                insert_after  -> {[], z_render:insert_after_selector(Sel, Html, Context)};
                iframe        -> {[], z_render:update_iframe(TargetId, Html, Context)}
            end
    end.

render_static(Method, TargetId, Html, Args, Context) ->
    Sel = z_render:css_selector(TargetId, Args),
    case proplists:get_value(appear, Args) of
        true ->
            case Method of
                update        -> {z_render:appear_selector_js(Sel, Html), Context};
                replace       -> {z_render:appear_replace_selector_js(Sel, Html), Context};
                insert_top    -> {z_render:appear_top_selector_js(Sel, Html), Context};
                insert_bottom -> {z_render:appear_bottom_selector_js(Sel, Html), Context};
                insert_before -> {z_render:appear_before_selector_js(Sel, Html), Context};
                insert_after  -> {z_render:appear_after_selector_js(Sel, Html), Context}
            end;
        _ ->
            case Method of
                update        -> {z_render:update_selector_js(Sel, Html), Context};
                    replace   -> {z_render:replace_selector_js(Sel, Html), Context};
                insert_top    -> {z_render:insert_top_selector_js(Sel, Html), Context};
                insert_bottom -> {z_render:insert_bottom_selector_js(Sel, Html), Context};
                insert_before -> {z_render:insert_before_selector_js(Sel, Html), Context};
                insert_after  -> {z_render:insert_after_selector_js(Sel, Html), Context}
            end
    end.

render_postback(Method, TriggerId, TargetId, Html, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({render, Method, Html, Args}, undefined, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

event(#postback{message={render, Method, Html, Args}, target=TargetId}, Context) ->
    {[], Context1} = render_inline(Method, TargetId, Html, Args, Context),
    Context1.

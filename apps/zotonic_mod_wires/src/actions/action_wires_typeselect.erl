%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%% @doc Adds typeahead with a searchresult to an input box

%% Copyright 2009-2021 Marc Worrell
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

-module(action_wires_typeselect).
-moduledoc("
Show possible selections whilst typing.

Performs a search for the typed text whilst typing in an input field. Shows possible matching pages in a selectable list.

Example:


```erlang
<form method=\"get\" action=\"/search\">
  <input type=\"search\" id=\"person\" name=\"person\" value=\"\" />
  <ul id=\"suggestions\"></ul>
  <input type=\"hidden\" id=\"person_id\" value=\"\" />
  {% wire id=\"person\" type=\"input\"
          action={typeselect cat=\"person\"
                             target=\"suggestions\"
                             action_with_id={with_args action={set_value target=\"person_id\"} arg={value select_id}}
                             action={submit}}
  %}
</form>
```

This is a rather complicated example. It connects the typeahead action to the input element. The list of suggestions
will be shown in the &lt;ul/> with id suggestions. Only pages in the category person will be found.

The listed suggestions will have two actions attached. One action will set the value of the hidden input element
person\\_id to the id of the selected suggestion (which is a [page](/id/doc_glossary#term-page)). The other action will
submit the form.

The action\\_with\\_id arguments are always performed before the action arguments.

The typeselect action accepts the following arguments:

| Argument           | Description                                                                      | Example                                                    |
| ------------------ | -------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| target             | The id of element that will show the list of suggestions.                        | target=”mylist”                                            |
| cat                | The category for the searched pages. This argument can be repeated.              | cat=”text”                                                 |
| template           | Template used to show the list of possible pages. This defaults to the template “\\\\_action\\\\_typeselect\\\\_result.tpl”. The template gets the following arguments: result (list of ids), action\\\\_with\\\\_id and action. | template=”\\\\_show\\\\_suggestions.tpl”                       |
| action\\\\_with\\\\_id | Actions executed when a suggestion is selected. The id of the selected page will be added as the id parameter. This argument can be repeated. | action\\\\_with\\\\_id=\\\\{postback postback=”page\\\\_select”\\\\} |
| action             | Actions executed when a suggestion is selected. This list is executed after the action\\\\_with\\\\_id actions. This argument can be repeated. | action=\\\\{slide\\\\_up target=”form-id”\\\\}                   |
").
-author("Marc Worrell <marc@worrell.nl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions = proplists:get_all_values(action, Args),
    ActionsWithId = proplists:get_all_values(action_with_id, Args),
    Cats = proplists:get_all_values(cat, Args),
    Template = proplists:get_value(template, Args, "_action_typeselect_result.tpl"),
    OtherArgs = proplists:delete(action,
                    proplists:delete(action_with_id,
                        proplists:delete(cat,
                            proplists:delete(template, Args)))),
    Postback = {typeselect, Cats, Template, Actions, ActionsWithId, OtherArgs},
    {_PostbackMsgJS, PickledPostback} = z_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
    JS = [
        <<"z_typeselect(\"">>, TriggerId, $",$,,$", PickledPostback, <<"\");">>
    ],
    {JS, Context}.


%% @doc Show possible completions of the search text using a template.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={typeselect, Cats, Template, Actions, ActionsWithId, OtherArgs}, target=TargetId}, Context) ->
    Text = z_context:get_q(<<"triggervalue">>, Context),
    Props = #{
        <<"cat">> => Cats,
        <<"text">> => Text
    },
    Result = z_search:search(<<"autocomplete">>, Props, 1, 20, Context),
    Vars = [
        {result, Result#search_result{
            search_name = autocomplete,
            search_args = Props
        }},
        {action, Actions},
        {action_with_id, ActionsWithId}
        | OtherArgs
    ],
    Html = z_template:render(Template, Vars, Context),
    z_render:update(TargetId, Html, Context).

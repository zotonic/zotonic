%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% @doc Wire an action to an element.

%% Copyright 2009-2013 Marc Worrell
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

-module(scomp_wires_wire).
-moduledoc("
Connect actions and events to a HTML element.

The wire tag is the basis for most Ajax interaction on web pages. It allows to connect actions to HTML elements.
Examples of [Actions](/id/template_action#actions) are [show](/id/doc_template_action_action_show) /
[hide](/id/doc_template_action_action_hide) elements or [postback](/id/doc_template_action_action_postback) to the server.



Wire actions to an element
--------------------------

The primary use of wire is to connect an action to a HTML element or javascript event.

Example:


```erlang
{% wire id=\"show\" action={show target=\"message\"} %}
<a id=\"show\" href=\"#\">Click to show a message</a>
<p style=\"display: none\" id=\"message\">Hello World!</p>
```

A click on the link will show the message “Hello World!”.



Wire postbacks to the server
----------------------------

Wire can post an event back to the server. Use for this the “postback” argument:


```erlang
{% wire id=\"mybutton\" postback={hello world=\"round\"} %}
<button id=\"mybutton\">Post event to server</button>
```

When the button is clicked the event function of the page controller will be called as:


```erlang
event(#postback{message=Postback, trigger=TriggerId, target=TargetId}, Context).
```

Where “Postback” will be `{hello, [{world,<<\"round\"\\>\\>}]}` and both “TriggerId” and “TargetId” will be `<<\"mybutton\"\\>\\>`.



Wire form submit events
-----------------------

Wire is also used to connect a form to the event routine handling the form.

Example:


```erlang
{% wire id=\"myform\" type=\"submit\" postback=\"some_tag\" action={toggle target=\"message\"} %}
<form id=\"myform\" method=\"post\" action=\"postback\">
  <input type=\"text\" name=\"title\" value=\"\" />
  <button id=\"mybutton\" type=\"submit\">Submit</button>
</form>
```

The wire tag redirects the submit of the form to the event routine of the controller. A submit will also toggle the
visibility of the “message” element on the page. Note that the action is “postback”, this is obligatory.

The event routine will be called as:


```erlang
event(#submit{message=Tag, form=FormId, target=TargetId}, Context).
```

Where Tag will be the postback set by the wire tag (in the example the atom `some_tag`) and FormId and TargetId are both
the HTML id of the submitted form. The posted input fields can be fetched using `z_context:get_q/2`,
`z_context:get_q_all/1` or `z_context:get_q_validated/2`.


```erlang
Title = z_context:get_q(<<\"title\">>, Context),
AllArgs = z_context:get_q_all(Context);
```

There are some extra arguments added to every form post:

| Post argument    | Description                                                                      | Example                |
| ---------------- | -------------------------------------------------------------------------------- | ---------------------- |
| z\\\\_trigger\\\\_id | Id of the HTML element that triggered the submit.                                | <<”mybutton” >>        |
| z\\\\_pageid       | Id of the page in the browser, used to connect comet and other communictation between the browser and the server. | <<”1uTsbzIsWqmPpF32”>> |
| postback         | Signed postback set by the wire tag. Handled internally.                         |                        |



Wire to the page load or unload
-------------------------------

A `{% wire %}` without an id will bind the actions and/or postback to the window instead of an element. Omitting a type
as well will execute all actions and/or postback on page load.

Example:


```erlang
{% wire action={alert text=\"Welcome to this page.\"} %}
```

The type “unload” will execute all actions and/or postback when leaving the page:


```erlang
{% wire type=\"unload\" action={alert text=\"Bye.\"} %}
```



Call a wire action from JavaScript
----------------------------------

Use `{% wire name=\"myname\" %}` to define a named action and trigger it from JavaScript with `z_event(\"myname\")`.
See: [Wires](/id/doc_developerguide_server_browser_interaction#guide-named-wire).



Wire an action to a MQTT topic
------------------------------

Note

[mod_mqtt](/id/doc_module_mod_mqtt) must be enabled before wiring to a topic

Use `{% wire type={mqtt topic=... topic=...} %}` to connect to one or more MQTT topics.

Example:


```erlang
{% wire type={mqtt topic=\"~site/public/hello\"} action={growl text=\"hello\"} %}
```

And in Erlang this will trigger the above *growl*:


```erlang
z_mqtt:publish(<<\"~site/public/hello\">>, <<>>, z_acl:sudo(z:c(mysite))).
```

See also [live](/id/doc_template_scomp_scomp_live#scomp-live)



Arguments
---------

The wire tag accepts the following arguments:

| Argument  | Description                                                                      | Example                                                 |
| --------- | -------------------------------------------------------------------------------- | ------------------------------------------------------- |
| id        | HTML id of the element the action gets connected to. When the id is not given then the event is bound to the window. | id=”mybutton”                                           |
| type      | The type of the event triggering the action. Defaults to “click”. Other types are: “enterkey”, “interval”, “continuation”, “submit” or one of the jQuery events “blur”, “focus”, “load”, “resize”, “scroll”, “unload”, “beforeunload”, “click”, “dblclick”, “mousedown”, “mouseup”, “mousemove”, “mouseover”, “mouseout”, “mouseenter”, “mouseleave”, “change”, “select”, “keydown”, “keypress”, “keyup” or “error”.  The types can be extended by modules using the `#action_event_type` notification. The type must be a tuple, an example is the `{mqtt topic=...}` type provided by [mod\\\\_mqtt](/id/doc_module_mod_mqtt) | type=”submit”                                           |
| propagate | Specify this when you don’t want the event to be canceled after handling the wire. Useful for event types like focus, click etc. .. versionadded:: 0.6.1 | propagate                                               |
| target    | Possible target for the action. The meaning of this argument depends on the action, defaults to id. |                                                         |
| action    | Action wired to the element. This parameter can be repeated to wire more than one action at a time. The value is a single or a list of action records. | action=\\\\{toggle target=”message”\\\\}                    |
| postback  | Postback that will be sent to the event handler of the controller or the delegate. Either a string, which will be send as an atom, or a tagged property list. The example will be in Erlang `{myevent, [{foo,1},{bar,2}]}`. | postback=”ajaxevent” postback=\\\\{myevent foo=1 bar=2\\\\} |
| delegate  | Name of the Erlang module that will receive the postback. Defaults to the controller that handled the page request. | delegate=”event\\\\_handler”                              |

See also

the tag [wire_args](/id/doc_template_scomp_scomp_wire_args#scomp-wire-args) and the list of predefined [Actions](/id/template_action#actions).
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id        = proplists:get_value(id, Params, 'window'),
    Type      = proplists:get_value(type,Params, case Id of 'window' -> inline; _ -> click end),
    TargetId  = proplists:get_value(target,Params,Id),
    Actions   = proplists:get_all_values(action,Params),
    Postback  = proplists:get_value(postback,Params),
    Delegate  = proplists:get_value(delegate,Params),
    Propagate = z_convert:to_bool(proplists:get_value(propagate, Params, false)),

    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                    undefined -> Options;
                    Postback  -> [{postback,Postback} | Options]
                end,
    Options2  = case proplists:get_value(name, Params) of
                    undefined -> [{type,Type}|Options1];
                    Name -> [{type,named},{name,Name}|Options1]
                end,
    Options3  = case proplists:get_value(propagate, Params) of
                    undefined -> Options2;
                    _ -> [{propagate,Propagate}|Options2]
                end,
    Options4  = [ {qarg,X} || {qarg,X} <- Params ] ++ Options3,
    Delegate1 = case Delegate of
                    undefined -> undefined;
                    _ -> z_convert:to_atom(Delegate)
                end,

    case Options1 of
        [] -> {error, "scomp wire: please give either an <em>action</em> or a <em>postback</em> parameter."};
        _  -> {ok, z_render:wire(Id, TargetId, {event,[{delegate,Delegate1}|Options4]}, Context)}
    end.

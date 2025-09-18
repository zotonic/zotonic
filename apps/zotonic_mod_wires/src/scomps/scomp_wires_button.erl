%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Generate a button with actions. Based on code (c) 2008-2009 Rusty Klophaus
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(scomp_wires_button).
-moduledoc("
Makes a button with an action attached.

This is an easy way to make a button and attach one or more actions or a postback.

For example:


```django
{% button text=\"Click me\" action={alert text=\"Hello Word!\"} %}
```

This show a button with the text “Click me”. When clicked it will trigger the
[alert](/id/doc_template_action_action_alert) action, showing an alert message with the text “Hello World!”.

Another example:


```django
{% button text=\"Postback\" postback={my_postback arg1=1 arg2=2} %}
```

When clicked it will call the `event/2` function in the controller that served the page. The function will be called as:


```erlang
event(#postback{message={mypostback, [{arg1,1}, {arg2,2}]},
    trigger=TriggerId, target=TargetId}, Context)
```

Where TriggerId and TargetId are both the HTML id of the button.

`button` accepts the following arguments:

| Argument | Description                                                                      | Example                        |
| -------- | -------------------------------------------------------------------------------- | ------------------------------ |
| text     | The text on the button, defaults to “Submit”                                     | text=”click me”                |
| postback | An event sent to the delegate or the resource serving the page.                  | postback=”hello”               |
| tag      | The type of HTML tag that will be created. Defaults to “button”.                 | tag=”a”                        |
| delegate | The name of the erlang module to be called for handling the postback.            | delegate=”myresource”          |
| action   | The action to be triggered when the button is clicked. There can be more than one action argument. | action=\\\\{show target=”msg”\\\\} |
| id       | Id of the button.                                                                | id=#submit                     |
| class    | The css class of the button. This argument can be repeated to add multiple classes. | class=”submit”                 |
| style    | The css style of the button.                                                     | style=”color: #fc0”            |
| tabindex | The value for the tabindex property.                                             | tabindex=1                     |
| type     | The type attribute of the button.                                                | type=”submit”                  |
| title    | The title attribute of the button.                                               | title=”click to submit”        |
| disabled | The disabled attribute of the button, set to true or false. When the button is disabled then the class “disabled” id added to the class list. | disabled=true                  |
| href     | The href attribute of the button if tag “a” is used. Defaults to “#”.            | href=”/foobar”                 |
").
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.
render(Params, _Vars, Context) ->
    Postback  = proplists:get_value(postback, Params),
	Delegate  = proplists:get_value(delegate, Params),
    Text      = proplists:get_value(text, Params, ?__("Submit", Context)),
    Id        = iolist_to_binary(z_ids:optid(proplists:get_value(id, Params))),
    Class     = proplists:get_all_values(class, Params),
    Icon      = proplists:get_all_values(icon, Params),
    Style     = proplists:get_value(style, Params),
    TabIndex  = proplists:get_value(tabindex, Params),
    Type      = proplists:get_value(type, Params, <<"button">>),
    Title     = proplists:get_value(title, Params),
    Disabled  = proplists:get_value(disabled, Params, false),
    Actions   = proplists:get_all_values(action, Params),
    Tag       = proplists:get_value(tag, Params, <<"button">>),
    Href      = proplists:get_value(href, Params, <<"#">>),

    Class1 = case Class of
        [] -> <<"btn btn-default">>;
        _ -> Class
    end,

    Options   = [{action,X} || X <- Actions],
    Options1  = if
        Postback =:= undefined -> Options;
        true -> [{postback,Postback} | Options]
    end,

    Context1 = if
        Options1 =:= [] ->
            Context;
        true  ->
            Options2  = if
               Delegate =:= undefined -> Options1;
	           true -> [{delegate, Delegate} | Options1]
            end,
            Options3  = [ {qarg,X} || {qarg,X} <- Params ] ++ Options2,
            z_render:wire(Id, {event,[{type,click}|Options3]}, Context)
    end,

    Attrs = [
        {<<"id">>,    Id},
        {<<"name">>,  case proplists:is_defined(id, Params) of true -> Id; false -> <<>> end},
        {<<"style">>, Style},
        {<<"title">>, Title},
        {<<"tabindex">>, TabIndex}
    ],

    {Class2, Attrs1} = case z_convert:to_bool(Disabled) of
        false -> {Class1, Attrs};
        true -> { [<<"disabled">> | Class1], [{<<"disabled">>,<<"disabled">>} | Attrs] }
    end,

    Attrs2 = if
        Type =:= undefined -> Attrs1;
        true -> [ {<<"type">>, Type} | Attrs1 ]
    end,
    Attrs3 = if
        Tag =:= <<"a">> -> [ {<<"href">>, Href} | Attrs2 ];
        true -> Attrs2
    end,
    Text1 = case z_utils:is_empty(Icon) of
        true -> Text;
        false -> [ z_tags:render_tag(<<"i">>, [{class, Icon}], <<>>), <<" ">>, Text ]
    end,
    Context2 = z_tags:render_tag(
                        Tag,
                        [ {<<"class">>,Class2} | Attrs3 ],
                    	Text1,
                    	Context1),
    {ok, Context2}.

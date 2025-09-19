%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell

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

-module(action_wires_postback).
-moduledoc("
This action sends a message to the event handler on the server.

Example:


```erlang
{% button text=\"Go\" action={postback postback=\"go\" action={growl text=\"sent message\"}} %}
```

Note

The [button](/id/doc_template_scomp_scomp_button#scomp-button) scomp can also take a postback argument directly.

After clicking the button the event go will be sent to the [controller](/id/doc_glossary#term-controller) module on the
server and a [growl](/id/doc_template_action_action_growl) message will be displayed.

The event/2 function in the controller module will be called as:


```erlang
event(#postback{message=go, trigger=TriggerId, target=TargetId}, Context)
```

This action can have the following arguments:

| Argument      | Description                                                                      | Example                                                                          |
| ------------- | -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| postback      | The message that will be send to the server module.                              | postback=\\\\{my\\\\_message arg=”hello”\\\\}                                          |
| delegate      | The name of the Erlang module that will be called. Defaults to the controller module generating the page. | delegate=”my\\\\_module”                                                           |
| action        | Any other actions that will be executed when the postback is done. This parameter can be repeated. | action=\\\\{show target=”wait”\\\\}                                                  |
| inject\\\\_args | If set to true, and postback is a tuple (as in the my\\\\_message example in this table), any values from the args in the postback will replace the arg value in the postback argument. This is useful when the arg is coming from an outer action and not set explicitly in the template code (as is done in the example for illustration). The value of some\\\\_arg in the postback handler will be 123. | \\\\{postback postback=\\\\{my\\\\_event some\\\\_arg\\\\} inject\\\\_args some\\\\_arg=123\\\\} |
| qarg          | Post the value of an input or select with the postback. The value of the qarg argument is the id of the element to be posted. Multiple qarg arguments can be given. On the server the value will be available as a normal query argument using z\\\\_context:get\\\\_q/2 | qarg=”my-input-id”                                                               |

New in version 0.9.0: Added inject\\_args option.
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	Postback0 = proplists:get_value(postback, Args),
    Delegate  = z_convert:to_atom(proplists:get_value(delegate, Args)),
    Actions   = proplists:get_all_values(action, Args),
    QArgs     = proplists:get_all_values(qarg, Args),

    %% check for injection of args into postback message
    Postback  = case {Postback0, proplists:get_value(inject_args, Args)} of
                    {{PbTag, PbArgs}, true} ->
                        {PbTag, lists:map(
                                 fun({K, V}) ->
                                         {K, proplists:get_value(K, Args, V)}
                                 end, PbArgs)};
                    {Pb, _} -> Pb
                end,

	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, undefined, TriggerId, TargetId,
                                                               Delegate, QArgs, Context),
	{ActionsJS, Context1} = z_render:render_actions(TriggerId, TargetId, Actions, Context),
	{[ PostbackMsgJS, ActionsJS ], Context1}.

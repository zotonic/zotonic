%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Postback validator, checks using a server callback against an Erlang function.

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

-module(validator_base_postback).
-moduledoc("
See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators)

Performs a custom server side validation of an input value. This allows you to add your own validation logic to HTML
form fields.

Start by adding a validator of the type `postback` to your form field in your template:


```django
<input type=\"text\" id=\"username\" name=\"username\" value=\"\" />
{% validate id=\"username\" type={postback event=\"validate_username\"} %}
```

The `event` argument declares the name of the event that will be
[notified](/id/doc_developerguide_notifications#guide-notification). [Handle this
event](/id/doc_developerguide_notifications#handling-notifications) in your site or module:

sites/yoursite/yoursite.erl
```erlang
-export([
    observe_validate_username/2
]).

%% The event name passed in your template as event=\"validate_username\",
%% prefixed with observe_
observe_validate_username({validate_username, {postback, Id, Value, _Args}}, Context) ->
    case is_valid(Value) of
        true ->
            {{ok, Value}, Context};
        false ->
            %% The validation message will be shown in the form
            {{error, Id, \"Sorry, that's not valid. Try again!\"}, Context}
    end.

%% Some function that returns true or false depending on the validity of the
%% input value
is_valid(Value) ->
    %% ...
```
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_validator/5,
    validate/5,
    event/2
]).

render_validator(postback, TriggerId, TargetId, Args, Context)  ->
	{_PostbackJS, PostbackInfo} = z_render:make_postback({validate, Args}, 'postback', TriggerId, TargetId, ?MODULE, Context),
	JsObject = z_utils:js_object(z_validation:rename_args([{z_postback, PostbackInfo}|Args])),
	Script = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"postback\", ">>, JsObject, <<");\n">>],
	{Args, Script}.

%% @spec validate(Type, TriggerId, Value, Args, Context) -> {{ok,AcceptedValue}, NewContext} | {{error,Id,Error}, NewContext}
%%          Error = invalid | novalue | {script, Script} | novalidator | string()
validate(postback, Id, Value, Args, Context) ->
    case proplists:get_value(delegate, Args) of
        undefined ->
            case proplists:get_value(event, Args) of
                undefined ->
                    %% No validator defined, default to being safe
                    {{error, Id, novalidator}, Context};
                Event ->
                    case z_notifier:first({z_convert:to_atom(Event), {postback, Id, Value, Args}}, Context) of
                        undefined -> {{error, Id, invalid}, Context};
                        Result -> Result
                    end
            end;
        Delegate ->
            Delegate:validate(postback, Id, Value, Args, Context)
    end.


%% @doc Handle the validation during form entry.
event(#postback{message={validate, Args}, trigger=TriggerId}, Context) ->
    Value = z_context:get_q(triggervalue, Context),
    {IsValid,ContextValidated} = case validate(postback, TriggerId, Value, Args, Context) of
        {{ok, _},Context1} ->
            {<<"true">>, Context1};
        {{error, Id, _} = Error, Context1} ->
            {<<"false">>, z_validation:report_errors([{Id,Error}], Context1)}
    end,
    z_render:add_script(["z_async_validation_result('",TriggerId,"', ",IsValid,", '",z_utils:js_escape(Value),"');"], ContextValidated).

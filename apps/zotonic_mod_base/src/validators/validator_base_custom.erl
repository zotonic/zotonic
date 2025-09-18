%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Custom javascript based validators

%% Copyright 2011 Marc Worrell
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

-module(validator_base_custom).
-moduledoc("
See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators)

Support for custom client-side (JavaScript-based) validators.

This call simply adds a `z_add_validator()` JavaScript call which adds a custom validator based on the arguments given
in the validator.

Example:


```django
<input type=\"text\" name=\"foobar\" id=\"foobar\" value=\"\" />
{% validate id=\"foobar\" type={custom against=\"window.validate_foobar\"} %}
```

And then `validate_foobar` is defined as follows:


```javascript
function validate_foobar(value, args, isSubmit, submitTrigger)
{
    // ... some logic to check the value
    return true or false;
}
```

The `args` are available if the validation is added using the LiveValidation JavaScript API.

`isSubmit` is `true` if the validation is triggered by a form submit, if it was triggered by change or focus events then it is `false`.

`submitTrigger` is the DOM tree node triggering the possible form submit.

Note that this validation does not do any server side validation. Because there is no server side validation, the value
of the `input` element is not available via `z_context:get_q_validated/2` but only via `z_context:get_q/2`.
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(custom, TriggerId, _TargetId, Args, _Context)  ->
    JsObject   = z_utils:js_object(z_validation:rename_args(Args)),
    Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"custom\", ">>, JsObject, <<");\n">>],
    {[], Script}.

%% @spec validate(Type, Name, Values, Args, Context) -> {ok, AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue
validate(custom, _Id, Value, _Args, Context) ->
    {{ok, Value}, Context}.

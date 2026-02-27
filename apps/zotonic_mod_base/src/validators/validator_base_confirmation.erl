%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if the input is the same as another input.

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

-module(validator_base_confirmation).
-moduledoc("
Check if two inputs are the same.

Useful when a form requires double entry of a password or e-mail address to prevent typos.

Accepts an additional parameter name with the name of the other input field.

For example:


```django
<input type=\"password\" id=\"password\" name=\"password\" value=\"\" />
<input type=\"password\" id=\"password2\" name=\"password2\" value=\"\" />
{% validate id=\"password\" type={confirmation match=\"password2\"} %}
```



Arguments
---------

| Argument          | Description                                                                      | Example                           |
| ----------------- | -------------------------------------------------------------------------------- | --------------------------------- |
| match             | The id of the input field that should have the same value.                       | `match=\"field1\"`                  |
| failure\\\\_message | Message to be shown when the two fields are unequal. Defaults to “Does not match.” | `failure_message=\"Please retry.\"` |

See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators)").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(confirmation, TriggerId, _TargetId, Args, _Context)  ->
    MatchField = proplists:get_value(match, Args),
	JsObject   = z_utils:js_object(z_validation:rename_args(Args)),
	Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"confirmation\", ">>, JsObject, <<");\n">>],
	{[MatchField], Script}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(confirmation, Id, Value, [MatchField], Context) ->
    MatchValue = z_validation:get_q(MatchField, Context),
    if
        MatchValue =:= Value -> {{ok, Value}, Context};
        true -> {{error, Id, invalid}, Context}
    end.

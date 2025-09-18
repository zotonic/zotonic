%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if an input value is a number and within a certain range.
%%      At the moment this function only accepts integers

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

-module(validator_base_numericality).
-moduledoc("
See also

[Forms and validation](/id/doc_developerguide_forms_and_validation#guide-validators)

Numerical input and range check.

Checks if the input is a number and within a certain range or equal to a fixed value. At the moment only integer inputs
are allowed.

Arguments are is, minimum and maximum.

For example, when the input must be 42:


```django
<input type=\"text\" id=\"number\" name=\"number\" value=\"\" />
{% validate id=\"number\" type={numericality is=42} %}
```

And for a number within a certain range:


```django
<input type=\"text\" id=\"percent\" name=\"percent\" value=\"\" />
{% validate id=\"percent\" type={numericality minimum=0 maximum=100} %}
```



Arguments
---------

| Argument                     | Description                                                                      | Example                    |
| ---------------------------- | -------------------------------------------------------------------------------- | -------------------------- |
| is                           | Tests for equality.                                                              | `is=42`                    |
| minimum                      | Minimum value.                                                                   | `minimum=1`                |
| maximum                      | Maximum value.                                                                   | `maximum=100`              |
| is\\\\_float                   | Boolean flag which tells if the input can be a floating point number. Defaults to false. | `is_float` `is_float=true` |
| not\\\\_a\\\\_number\\\\_message   | Message to show when the entered value is not a number. Defaults to “Must be a number.” | `not_a_number_message=\"*\"` |
| not\\\\_an\\\\_integer\\\\_message | Message to show when the entered number is not an integer. Defaults to “Must be an integer.” |                            |
| wrong\\\\_number\\\\_message     | Message to show when the entered number is unequal to the .is. argument. Defaults to “Must be ..” |                            |
| too\\\\_low\\\\_message          | Message for when the entered number is less than the minimum allowed. Defaults to “Must not be less than ..” |                            |
| too\\\\_high\\\\_message         | Message for when the entered number is greater than the maximum allowed. Defaults to “Must not be more than ..” |                            |
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(numericality, TriggerId, _TargetId, Args, _Context)  ->
    Is = proplists:get_value(is, Args),
    {Min,Max} = case Is of
                    undefined -> { proplists:get_value(minimum, Args), proplists:get_value(maximum, Args) };
                    _ -> {Is,Is}
                end,
    IsFloat = z_convert:to_bool(proplists:get_value(is_float, Args)),
    Opt = case IsFloat of
              true -> [];
              false -> [{onlyInteger,true}]
          end,
	JsObject   = z_utils:js_object(Opt ++ z_validation:rename_args(Args)),
	Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"numericality\", ">>, JsObject, <<");\n">>],
	{[IsFloat, to_number(Min),to_number(Max)], Script}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(numericality, Id, Value, [IsFloat,Min,Max], Context) ->
    Result = case z_string:trim(Value) of
                 <<>> -> {ok, <<>>};
                 Trimmed ->
                     ConvertFun = case IsFloat of
                                      true -> fun z_convert:to_float/1;
                                      false -> fun z_convert:to_integer/1
                                  end,
                     try
                         validate_minmax(Trimmed, ConvertFun(Trimmed), Min, Max, Id)
                     catch
                         error:{badarg, _} ->
                             {error, Id, invalid}
                     end
            end,
    {Result, Context}.

validate_minmax(Value, Num, Min, Max, Id) ->
    MinOK = (Min == -1 orelse Num >= Min),
    MaxOK = (Max == -1 orelse Num =< Max),
    case MinOK andalso MaxOK of
        true  -> {ok, Value};
        false -> {error, Id, invalid}
    end.

to_number(undefined) -> -1;
to_number(N) when is_float(N) -> round(N);
to_number(N) when is_integer(N) -> N;
to_number(N) -> z_convert:to_integer(N).

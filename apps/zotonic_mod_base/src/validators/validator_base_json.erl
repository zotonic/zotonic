%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Validate the input to be valid JSON
%% @end

%% Copyright 2023 Marc Worrell
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

-module(validator_base_json).
-moduledoc("
Todo

Not yet documented.
").

-export([render_validator/5, validate/5]).

render_validator(json, TriggerId, _TargetId, Args, _Context)  ->
    JsObject = z_utils:js_object(z_validation:rename_args(Args)),
    Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"json\", ">>, JsObject, <<");\n">>],
    {[], Script}.

validate(json, Id, Value, _Args, Context) ->
    case z_string:trim(Value) of
        <<>> ->
            {{ok, <<>>}, Context};
        Trimmed ->
            try
                jsxrecord:decode(Trimmed),
                {{ok, Trimmed}, Context}
            catch
                _:_ ->
                    {{error, Id, invalid}, Context}
            end
    end.

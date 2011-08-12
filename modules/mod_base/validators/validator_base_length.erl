%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Validator for checking if an input value is within a certain length range

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

-module(validator_base_length).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(length, TriggerId, _TargetId, Args, Context)  ->
    Min        = proplists:get_value(minimum, Args),
    Max        = proplists:get_value(maximum, Args),
	JsObject   = z_utils:js_object(z_validation:rename_args(Args)),
	Script     = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"length\", ">>, JsObject, <<");\n">>],
	{[to_number(Min),to_number(Max)], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptedValue} | {error,Id,Error}
%%          Error = invalid | novalue | {script, Script}
validate(length, Id, Value, [Min,Max], Context) ->
    Len   = length(Value),
    MinOK = (Min == -1 orelse Len >= Min),
    MaxOK = (Max == -1 orelse Len =< Max),
    case MinOK andalso MaxOK of
        true  -> {{ok, Value}, Context};
        false -> {{error, Id, invalid}, Context}
    end.

to_number(undefined) -> 
    -1;
to_number(N) when is_integer(N) -> 
    N;
to_number(N) -> 
    {I,_Rest} = string:to_integer(N),
    I.
